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
-- Module      : Amazonka.EC2.DisableFastLaunch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Discontinue faster launching for a Windows AMI, and clean up existing
-- pre-provisioned snapshots. When you disable faster launching, the AMI
-- uses the standard launch process for each instance. All pre-provisioned
-- snapshots must be removed before you can enable faster launching again.
--
-- To change these settings, you must own the AMI.
module Amazonka.EC2.DisableFastLaunch
  ( -- * Creating a Request
    DisableFastLaunch (..),
    newDisableFastLaunch,

    -- * Request Lenses
    disableFastLaunch_dryRun,
    disableFastLaunch_force,
    disableFastLaunch_imageId,

    -- * Destructuring the Response
    DisableFastLaunchResponse (..),
    newDisableFastLaunchResponse,

    -- * Response Lenses
    disableFastLaunchResponse_imageId,
    disableFastLaunchResponse_launchTemplate,
    disableFastLaunchResponse_maxParallelLaunches,
    disableFastLaunchResponse_ownerId,
    disableFastLaunchResponse_resourceType,
    disableFastLaunchResponse_snapshotConfiguration,
    disableFastLaunchResponse_state,
    disableFastLaunchResponse_stateTransitionReason,
    disableFastLaunchResponse_stateTransitionTime,
    disableFastLaunchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableFastLaunch' smart constructor.
data DisableFastLaunch = DisableFastLaunch'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Forces the image settings to turn off faster launching for your Windows
    -- AMI. This parameter overrides any errors that are encountered while
    -- cleaning up resources in your account.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the image for which you’re turning off faster launching, and
    -- removing pre-provisioned snapshots.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableFastLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableFastLaunch_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'force', 'disableFastLaunch_force' - Forces the image settings to turn off faster launching for your Windows
-- AMI. This parameter overrides any errors that are encountered while
-- cleaning up resources in your account.
--
-- 'imageId', 'disableFastLaunch_imageId' - The ID of the image for which you’re turning off faster launching, and
-- removing pre-provisioned snapshots.
newDisableFastLaunch ::
  -- | 'imageId'
  Prelude.Text ->
  DisableFastLaunch
newDisableFastLaunch pImageId_ =
  DisableFastLaunch'
    { dryRun = Prelude.Nothing,
      force = Prelude.Nothing,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableFastLaunch_dryRun :: Lens.Lens' DisableFastLaunch (Prelude.Maybe Prelude.Bool)
disableFastLaunch_dryRun = Lens.lens (\DisableFastLaunch' {dryRun} -> dryRun) (\s@DisableFastLaunch' {} a -> s {dryRun = a} :: DisableFastLaunch)

-- | Forces the image settings to turn off faster launching for your Windows
-- AMI. This parameter overrides any errors that are encountered while
-- cleaning up resources in your account.
disableFastLaunch_force :: Lens.Lens' DisableFastLaunch (Prelude.Maybe Prelude.Bool)
disableFastLaunch_force = Lens.lens (\DisableFastLaunch' {force} -> force) (\s@DisableFastLaunch' {} a -> s {force = a} :: DisableFastLaunch)

-- | The ID of the image for which you’re turning off faster launching, and
-- removing pre-provisioned snapshots.
disableFastLaunch_imageId :: Lens.Lens' DisableFastLaunch Prelude.Text
disableFastLaunch_imageId = Lens.lens (\DisableFastLaunch' {imageId} -> imageId) (\s@DisableFastLaunch' {} a -> s {imageId = a} :: DisableFastLaunch)

instance Core.AWSRequest DisableFastLaunch where
  type
    AWSResponse DisableFastLaunch =
      DisableFastLaunchResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableFastLaunchResponse'
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

instance Prelude.Hashable DisableFastLaunch where
  hashWithSalt _salt DisableFastLaunch' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData DisableFastLaunch where
  rnf DisableFastLaunch' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders DisableFastLaunch where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DisableFastLaunch where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableFastLaunch where
  toQuery DisableFastLaunch' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DisableFastLaunch" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Force" Data.=: force,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newDisableFastLaunchResponse' smart constructor.
data DisableFastLaunchResponse = DisableFastLaunchResponse'
  { -- | The ID of the image for which faster-launching has been turned off.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The launch template that was used to launch Windows instances from
    -- pre-provisioned snapshots.
    launchTemplate :: Prelude.Maybe FastLaunchLaunchTemplateSpecificationResponse,
    -- | The maximum number of parallel instances to launch for creating
    -- resources.
    maxParallelLaunches :: Prelude.Maybe Prelude.Int,
    -- | The owner of the Windows AMI for which faster launching was turned off.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The pre-provisioning resource type that must be cleaned after turning
    -- off faster launching for the Windows AMI. Supported values include:
    -- @snapshot@.
    resourceType :: Prelude.Maybe FastLaunchResourceType,
    -- | Parameters that were used for faster launching for the Windows AMI
    -- before faster launching was turned off. This informs the clean-up
    -- process.
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
-- Create a value of 'DisableFastLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'disableFastLaunchResponse_imageId' - The ID of the image for which faster-launching has been turned off.
--
-- 'launchTemplate', 'disableFastLaunchResponse_launchTemplate' - The launch template that was used to launch Windows instances from
-- pre-provisioned snapshots.
--
-- 'maxParallelLaunches', 'disableFastLaunchResponse_maxParallelLaunches' - The maximum number of parallel instances to launch for creating
-- resources.
--
-- 'ownerId', 'disableFastLaunchResponse_ownerId' - The owner of the Windows AMI for which faster launching was turned off.
--
-- 'resourceType', 'disableFastLaunchResponse_resourceType' - The pre-provisioning resource type that must be cleaned after turning
-- off faster launching for the Windows AMI. Supported values include:
-- @snapshot@.
--
-- 'snapshotConfiguration', 'disableFastLaunchResponse_snapshotConfiguration' - Parameters that were used for faster launching for the Windows AMI
-- before faster launching was turned off. This informs the clean-up
-- process.
--
-- 'state', 'disableFastLaunchResponse_state' - The current state of faster launching for the specified Windows AMI.
--
-- 'stateTransitionReason', 'disableFastLaunchResponse_stateTransitionReason' - The reason that the state changed for faster launching for the Windows
-- AMI.
--
-- 'stateTransitionTime', 'disableFastLaunchResponse_stateTransitionTime' - The time that the state changed for faster launching for the Windows
-- AMI.
--
-- 'httpStatus', 'disableFastLaunchResponse_httpStatus' - The response's http status code.
newDisableFastLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableFastLaunchResponse
newDisableFastLaunchResponse pHttpStatus_ =
  DisableFastLaunchResponse'
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

-- | The ID of the image for which faster-launching has been turned off.
disableFastLaunchResponse_imageId :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe Prelude.Text)
disableFastLaunchResponse_imageId = Lens.lens (\DisableFastLaunchResponse' {imageId} -> imageId) (\s@DisableFastLaunchResponse' {} a -> s {imageId = a} :: DisableFastLaunchResponse)

-- | The launch template that was used to launch Windows instances from
-- pre-provisioned snapshots.
disableFastLaunchResponse_launchTemplate :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe FastLaunchLaunchTemplateSpecificationResponse)
disableFastLaunchResponse_launchTemplate = Lens.lens (\DisableFastLaunchResponse' {launchTemplate} -> launchTemplate) (\s@DisableFastLaunchResponse' {} a -> s {launchTemplate = a} :: DisableFastLaunchResponse)

-- | The maximum number of parallel instances to launch for creating
-- resources.
disableFastLaunchResponse_maxParallelLaunches :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe Prelude.Int)
disableFastLaunchResponse_maxParallelLaunches = Lens.lens (\DisableFastLaunchResponse' {maxParallelLaunches} -> maxParallelLaunches) (\s@DisableFastLaunchResponse' {} a -> s {maxParallelLaunches = a} :: DisableFastLaunchResponse)

-- | The owner of the Windows AMI for which faster launching was turned off.
disableFastLaunchResponse_ownerId :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe Prelude.Text)
disableFastLaunchResponse_ownerId = Lens.lens (\DisableFastLaunchResponse' {ownerId} -> ownerId) (\s@DisableFastLaunchResponse' {} a -> s {ownerId = a} :: DisableFastLaunchResponse)

-- | The pre-provisioning resource type that must be cleaned after turning
-- off faster launching for the Windows AMI. Supported values include:
-- @snapshot@.
disableFastLaunchResponse_resourceType :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe FastLaunchResourceType)
disableFastLaunchResponse_resourceType = Lens.lens (\DisableFastLaunchResponse' {resourceType} -> resourceType) (\s@DisableFastLaunchResponse' {} a -> s {resourceType = a} :: DisableFastLaunchResponse)

-- | Parameters that were used for faster launching for the Windows AMI
-- before faster launching was turned off. This informs the clean-up
-- process.
disableFastLaunchResponse_snapshotConfiguration :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe FastLaunchSnapshotConfigurationResponse)
disableFastLaunchResponse_snapshotConfiguration = Lens.lens (\DisableFastLaunchResponse' {snapshotConfiguration} -> snapshotConfiguration) (\s@DisableFastLaunchResponse' {} a -> s {snapshotConfiguration = a} :: DisableFastLaunchResponse)

-- | The current state of faster launching for the specified Windows AMI.
disableFastLaunchResponse_state :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe FastLaunchStateCode)
disableFastLaunchResponse_state = Lens.lens (\DisableFastLaunchResponse' {state} -> state) (\s@DisableFastLaunchResponse' {} a -> s {state = a} :: DisableFastLaunchResponse)

-- | The reason that the state changed for faster launching for the Windows
-- AMI.
disableFastLaunchResponse_stateTransitionReason :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe Prelude.Text)
disableFastLaunchResponse_stateTransitionReason = Lens.lens (\DisableFastLaunchResponse' {stateTransitionReason} -> stateTransitionReason) (\s@DisableFastLaunchResponse' {} a -> s {stateTransitionReason = a} :: DisableFastLaunchResponse)

-- | The time that the state changed for faster launching for the Windows
-- AMI.
disableFastLaunchResponse_stateTransitionTime :: Lens.Lens' DisableFastLaunchResponse (Prelude.Maybe Prelude.UTCTime)
disableFastLaunchResponse_stateTransitionTime = Lens.lens (\DisableFastLaunchResponse' {stateTransitionTime} -> stateTransitionTime) (\s@DisableFastLaunchResponse' {} a -> s {stateTransitionTime = a} :: DisableFastLaunchResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
disableFastLaunchResponse_httpStatus :: Lens.Lens' DisableFastLaunchResponse Prelude.Int
disableFastLaunchResponse_httpStatus = Lens.lens (\DisableFastLaunchResponse' {httpStatus} -> httpStatus) (\s@DisableFastLaunchResponse' {} a -> s {httpStatus = a} :: DisableFastLaunchResponse)

instance Prelude.NFData DisableFastLaunchResponse where
  rnf DisableFastLaunchResponse' {..} =
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
