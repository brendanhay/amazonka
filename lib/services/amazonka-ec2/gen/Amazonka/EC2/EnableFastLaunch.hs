{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.EnableFastLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you enable faster launching for a Windows AMI, images are
-- pre-provisioned, using snapshots to launch instances up to 65% faster.
-- To create the optimized Windows image, Amazon EC2 launches an instance
-- and runs through Sysprep steps, rebooting as required. Then it creates a
-- set of reserved snapshots that are used for subsequent launches. The
-- reserved snapshots are automatically replenished as they are used,
-- depending on your settings for launch frequency.
--
-- To change these settings, you must own the AMI.
module Amazonka.EC2.EnableFastLaunch
  ( -- * Creating a Request
    EnableFastLaunch (..),
    newEnableFastLaunch,

    -- * Request Lenses
    enableFastLaunch_dryRun,
    enableFastLaunch_launchTemplate,
    enableFastLaunch_maxParallelLaunches,
    enableFastLaunch_resourceType,
    enableFastLaunch_snapshotConfiguration,
    enableFastLaunch_imageId,

    -- * Destructuring the Response
    EnableFastLaunchResponse (..),
    newEnableFastLaunchResponse,

    -- * Response Lenses
    enableFastLaunchResponse_imageId,
    enableFastLaunchResponse_launchTemplate,
    enableFastLaunchResponse_maxParallelLaunches,
    enableFastLaunchResponse_ownerId,
    enableFastLaunchResponse_resourceType,
    enableFastLaunchResponse_snapshotConfiguration,
    enableFastLaunchResponse_state,
    enableFastLaunchResponse_stateTransitionReason,
    enableFastLaunchResponse_stateTransitionTime,
    enableFastLaunchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableFastLaunch' smart constructor.
data EnableFastLaunch = EnableFastLaunch'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The launch template to use when launching Windows instances from
    -- pre-provisioned snapshots. Launch template parameters can include either
    -- the name or ID of the launch template, but not both.
    launchTemplate :: Prelude.Maybe FastLaunchLaunchTemplateSpecificationRequest,
    -- | The maximum number of parallel instances to launch for creating
    -- resources. Value must be @6@ or greater.
    maxParallelLaunches :: Prelude.Maybe Prelude.Int,
    -- | The type of resource to use for pre-provisioning the Windows AMI for
    -- faster launching. Supported values include: @snapshot@, which is the
    -- default value.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Configuration settings for creating and managing the snapshots that are
    -- used for pre-provisioning the Windows AMI for faster launching. The
    -- associated @ResourceType@ must be @snapshot@.
    snapshotConfiguration :: Prelude.Maybe FastLaunchSnapshotConfigurationRequest,
    -- | The ID of the image for which you’re enabling faster launching.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableFastLaunch_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplate', 'enableFastLaunch_launchTemplate' - The launch template to use when launching Windows instances from
-- pre-provisioned snapshots. Launch template parameters can include either
-- the name or ID of the launch template, but not both.
--
-- 'maxParallelLaunches', 'enableFastLaunch_maxParallelLaunches' - The maximum number of parallel instances to launch for creating
-- resources. Value must be @6@ or greater.
--
-- 'resourceType', 'enableFastLaunch_resourceType' - The type of resource to use for pre-provisioning the Windows AMI for
-- faster launching. Supported values include: @snapshot@, which is the
-- default value.
--
-- 'snapshotConfiguration', 'enableFastLaunch_snapshotConfiguration' - Configuration settings for creating and managing the snapshots that are
-- used for pre-provisioning the Windows AMI for faster launching. The
-- associated @ResourceType@ must be @snapshot@.
--
-- 'imageId', 'enableFastLaunch_imageId' - The ID of the image for which you’re enabling faster launching.
newEnableFastLaunch ::
  -- | 'imageId'
  Prelude.Text ->
  EnableFastLaunch
newEnableFastLaunch pImageId_ =
  EnableFastLaunch'
    { dryRun = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      maxParallelLaunches = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      snapshotConfiguration = Prelude.Nothing,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableFastLaunch_dryRun :: Lens.Lens' EnableFastLaunch (Prelude.Maybe Prelude.Bool)
enableFastLaunch_dryRun = Lens.lens (\EnableFastLaunch' {dryRun} -> dryRun) (\s@EnableFastLaunch' {} a -> s {dryRun = a} :: EnableFastLaunch)

-- | The launch template to use when launching Windows instances from
-- pre-provisioned snapshots. Launch template parameters can include either
-- the name or ID of the launch template, but not both.
enableFastLaunch_launchTemplate :: Lens.Lens' EnableFastLaunch (Prelude.Maybe FastLaunchLaunchTemplateSpecificationRequest)
enableFastLaunch_launchTemplate = Lens.lens (\EnableFastLaunch' {launchTemplate} -> launchTemplate) (\s@EnableFastLaunch' {} a -> s {launchTemplate = a} :: EnableFastLaunch)

-- | The maximum number of parallel instances to launch for creating
-- resources. Value must be @6@ or greater.
enableFastLaunch_maxParallelLaunches :: Lens.Lens' EnableFastLaunch (Prelude.Maybe Prelude.Int)
enableFastLaunch_maxParallelLaunches = Lens.lens (\EnableFastLaunch' {maxParallelLaunches} -> maxParallelLaunches) (\s@EnableFastLaunch' {} a -> s {maxParallelLaunches = a} :: EnableFastLaunch)

-- | The type of resource to use for pre-provisioning the Windows AMI for
-- faster launching. Supported values include: @snapshot@, which is the
-- default value.
enableFastLaunch_resourceType :: Lens.Lens' EnableFastLaunch (Prelude.Maybe Prelude.Text)
enableFastLaunch_resourceType = Lens.lens (\EnableFastLaunch' {resourceType} -> resourceType) (\s@EnableFastLaunch' {} a -> s {resourceType = a} :: EnableFastLaunch)

-- | Configuration settings for creating and managing the snapshots that are
-- used for pre-provisioning the Windows AMI for faster launching. The
-- associated @ResourceType@ must be @snapshot@.
enableFastLaunch_snapshotConfiguration :: Lens.Lens' EnableFastLaunch (Prelude.Maybe FastLaunchSnapshotConfigurationRequest)
enableFastLaunch_snapshotConfiguration = Lens.lens (\EnableFastLaunch' {snapshotConfiguration} -> snapshotConfiguration) (\s@EnableFastLaunch' {} a -> s {snapshotConfiguration = a} :: EnableFastLaunch)

-- | The ID of the image for which you’re enabling faster launching.
enableFastLaunch_imageId :: Lens.Lens' EnableFastLaunch Prelude.Text
enableFastLaunch_imageId = Lens.lens (\EnableFastLaunch' {imageId} -> imageId) (\s@EnableFastLaunch' {} a -> s {imageId = a} :: EnableFastLaunch)

instance Core.AWSRequest EnableFastLaunch where
  type
    AWSResponse EnableFastLaunch =
      EnableFastLaunchResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableFastLaunchResponse'
            Prelude.<$> (x Data..@? "imageId")
            Prelude.<*> (x Data..@? "launchTemplate")
            Prelude.<*> (x Data..@? "maxParallelLaunches")
            Prelude.<*> (x Data..@? "ownerId")
            Prelude.<*> (x Data..@? "resourceType")
            Prelude.<*> (x Data..@? "snapshotConfiguration")
            Prelude.<*> (x Data..@? "state")
            Prelude.<*> (x Data..@? "stateTransitionReason")
            Prelude.<*> (x Data..@? "stateTransitionTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableFastLaunch where
  hashWithSalt _salt EnableFastLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` maxParallelLaunches
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` snapshotConfiguration
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData EnableFastLaunch where
  rnf EnableFastLaunch' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf maxParallelLaunches
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf snapshotConfiguration
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders EnableFastLaunch where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableFastLaunch where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableFastLaunch where
  toQuery EnableFastLaunch' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableFastLaunch" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "LaunchTemplate" Data.=: launchTemplate,
        "MaxParallelLaunches" Data.=: maxParallelLaunches,
        "ResourceType" Data.=: resourceType,
        "SnapshotConfiguration"
          Data.=: snapshotConfiguration,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newEnableFastLaunchResponse' smart constructor.
data EnableFastLaunchResponse = EnableFastLaunchResponse'
  { -- | The image ID that identifies the Windows AMI for which faster launching
    -- was enabled.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The launch template that is used when launching Windows instances from
    -- pre-provisioned snapshots.
    launchTemplate :: Prelude.Maybe FastLaunchLaunchTemplateSpecificationResponse,
    -- | The maximum number of parallel instances to launch for creating
    -- resources.
    maxParallelLaunches :: Prelude.Maybe Prelude.Int,
    -- | The owner ID for the Windows AMI for which faster launching was enabled.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource that was defined for pre-provisioning the Windows
    -- AMI for faster launching.
    resourceType :: Prelude.Maybe FastLaunchResourceType,
    -- | The configuration settings that were defined for creating and managing
    -- the pre-provisioned snapshots for faster launching of the Windows AMI.
    -- This property is returned when the associated @resourceType@ is
    -- @snapshot@.
    snapshotConfiguration :: Prelude.Maybe FastLaunchSnapshotConfigurationResponse,
    -- | The current state of faster launching for the specified Windows AMI.
    state :: Prelude.Maybe FastLaunchStateCode,
    -- | The reason that the state changed for faster launching for the Windows
    -- AMI.
    stateTransitionReason :: Prelude.Maybe Prelude.Text,
    -- | The time that the state changed for faster launching for the Windows
    -- AMI.
    stateTransitionTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'enableFastLaunchResponse_imageId' - The image ID that identifies the Windows AMI for which faster launching
-- was enabled.
--
-- 'launchTemplate', 'enableFastLaunchResponse_launchTemplate' - The launch template that is used when launching Windows instances from
-- pre-provisioned snapshots.
--
-- 'maxParallelLaunches', 'enableFastLaunchResponse_maxParallelLaunches' - The maximum number of parallel instances to launch for creating
-- resources.
--
-- 'ownerId', 'enableFastLaunchResponse_ownerId' - The owner ID for the Windows AMI for which faster launching was enabled.
--
-- 'resourceType', 'enableFastLaunchResponse_resourceType' - The type of resource that was defined for pre-provisioning the Windows
-- AMI for faster launching.
--
-- 'snapshotConfiguration', 'enableFastLaunchResponse_snapshotConfiguration' - The configuration settings that were defined for creating and managing
-- the pre-provisioned snapshots for faster launching of the Windows AMI.
-- This property is returned when the associated @resourceType@ is
-- @snapshot@.
--
-- 'state', 'enableFastLaunchResponse_state' - The current state of faster launching for the specified Windows AMI.
--
-- 'stateTransitionReason', 'enableFastLaunchResponse_stateTransitionReason' - The reason that the state changed for faster launching for the Windows
-- AMI.
--
-- 'stateTransitionTime', 'enableFastLaunchResponse_stateTransitionTime' - The time that the state changed for faster launching for the Windows
-- AMI.
--
-- 'httpStatus', 'enableFastLaunchResponse_httpStatus' - The response's http status code.
newEnableFastLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableFastLaunchResponse
newEnableFastLaunchResponse pHttpStatus_ =
  EnableFastLaunchResponse'
    { imageId =
        Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      maxParallelLaunches = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      snapshotConfiguration = Prelude.Nothing,
      state = Prelude.Nothing,
      stateTransitionReason = Prelude.Nothing,
      stateTransitionTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The image ID that identifies the Windows AMI for which faster launching
-- was enabled.
enableFastLaunchResponse_imageId :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe Prelude.Text)
enableFastLaunchResponse_imageId = Lens.lens (\EnableFastLaunchResponse' {imageId} -> imageId) (\s@EnableFastLaunchResponse' {} a -> s {imageId = a} :: EnableFastLaunchResponse)

-- | The launch template that is used when launching Windows instances from
-- pre-provisioned snapshots.
enableFastLaunchResponse_launchTemplate :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe FastLaunchLaunchTemplateSpecificationResponse)
enableFastLaunchResponse_launchTemplate = Lens.lens (\EnableFastLaunchResponse' {launchTemplate} -> launchTemplate) (\s@EnableFastLaunchResponse' {} a -> s {launchTemplate = a} :: EnableFastLaunchResponse)

-- | The maximum number of parallel instances to launch for creating
-- resources.
enableFastLaunchResponse_maxParallelLaunches :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe Prelude.Int)
enableFastLaunchResponse_maxParallelLaunches = Lens.lens (\EnableFastLaunchResponse' {maxParallelLaunches} -> maxParallelLaunches) (\s@EnableFastLaunchResponse' {} a -> s {maxParallelLaunches = a} :: EnableFastLaunchResponse)

-- | The owner ID for the Windows AMI for which faster launching was enabled.
enableFastLaunchResponse_ownerId :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe Prelude.Text)
enableFastLaunchResponse_ownerId = Lens.lens (\EnableFastLaunchResponse' {ownerId} -> ownerId) (\s@EnableFastLaunchResponse' {} a -> s {ownerId = a} :: EnableFastLaunchResponse)

-- | The type of resource that was defined for pre-provisioning the Windows
-- AMI for faster launching.
enableFastLaunchResponse_resourceType :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe FastLaunchResourceType)
enableFastLaunchResponse_resourceType = Lens.lens (\EnableFastLaunchResponse' {resourceType} -> resourceType) (\s@EnableFastLaunchResponse' {} a -> s {resourceType = a} :: EnableFastLaunchResponse)

-- | The configuration settings that were defined for creating and managing
-- the pre-provisioned snapshots for faster launching of the Windows AMI.
-- This property is returned when the associated @resourceType@ is
-- @snapshot@.
enableFastLaunchResponse_snapshotConfiguration :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe FastLaunchSnapshotConfigurationResponse)
enableFastLaunchResponse_snapshotConfiguration = Lens.lens (\EnableFastLaunchResponse' {snapshotConfiguration} -> snapshotConfiguration) (\s@EnableFastLaunchResponse' {} a -> s {snapshotConfiguration = a} :: EnableFastLaunchResponse)

-- | The current state of faster launching for the specified Windows AMI.
enableFastLaunchResponse_state :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe FastLaunchStateCode)
enableFastLaunchResponse_state = Lens.lens (\EnableFastLaunchResponse' {state} -> state) (\s@EnableFastLaunchResponse' {} a -> s {state = a} :: EnableFastLaunchResponse)

-- | The reason that the state changed for faster launching for the Windows
-- AMI.
enableFastLaunchResponse_stateTransitionReason :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe Prelude.Text)
enableFastLaunchResponse_stateTransitionReason = Lens.lens (\EnableFastLaunchResponse' {stateTransitionReason} -> stateTransitionReason) (\s@EnableFastLaunchResponse' {} a -> s {stateTransitionReason = a} :: EnableFastLaunchResponse)

-- | The time that the state changed for faster launching for the Windows
-- AMI.
enableFastLaunchResponse_stateTransitionTime :: Lens.Lens' EnableFastLaunchResponse (Prelude.Maybe Prelude.UTCTime)
enableFastLaunchResponse_stateTransitionTime = Lens.lens (\EnableFastLaunchResponse' {stateTransitionTime} -> stateTransitionTime) (\s@EnableFastLaunchResponse' {} a -> s {stateTransitionTime = a} :: EnableFastLaunchResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
enableFastLaunchResponse_httpStatus :: Lens.Lens' EnableFastLaunchResponse Prelude.Int
enableFastLaunchResponse_httpStatus = Lens.lens (\EnableFastLaunchResponse' {httpStatus} -> httpStatus) (\s@EnableFastLaunchResponse' {} a -> s {httpStatus = a} :: EnableFastLaunchResponse)

instance Prelude.NFData EnableFastLaunchResponse where
  rnf EnableFastLaunchResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf maxParallelLaunches
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf snapshotConfiguration
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateTransitionReason
      `Prelude.seq` Prelude.rnf stateTransitionTime
      `Prelude.seq` Prelude.rnf httpStatus
