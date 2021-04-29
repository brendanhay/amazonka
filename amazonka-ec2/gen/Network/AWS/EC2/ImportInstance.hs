{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ImportInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import instance task using metadata from the specified disk
-- image. @ImportInstance@ only supports single-volume VMs. To import
-- multi-volume VMs, use ImportImage. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/ec2-cli-vmimport-export.html Importing a Virtual Machine Using the Amazon EC2 CLI>.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
module Network.AWS.EC2.ImportInstance
  ( -- * Creating a Request
    ImportInstance (..),
    newImportInstance,

    -- * Request Lenses
    importInstance_diskImages,
    importInstance_dryRun,
    importInstance_description,
    importInstance_launchSpecification,
    importInstance_platform,

    -- * Destructuring the Response
    ImportInstanceResponse (..),
    newImportInstanceResponse,

    -- * Response Lenses
    importInstanceResponse_conversionTask,
    importInstanceResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportInstance' smart constructor.
data ImportInstance = ImportInstance'
  { -- | The disk image.
    diskImages :: Prelude.Maybe [DiskImage],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A description for the instance being imported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The launch specification.
    launchSpecification :: Prelude.Maybe ImportInstanceLaunchSpecification,
    -- | The instance operating system.
    platform :: PlatformValues
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskImages', 'importInstance_diskImages' - The disk image.
--
-- 'dryRun', 'importInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'description', 'importInstance_description' - A description for the instance being imported.
--
-- 'launchSpecification', 'importInstance_launchSpecification' - The launch specification.
--
-- 'platform', 'importInstance_platform' - The instance operating system.
newImportInstance ::
  -- | 'platform'
  PlatformValues ->
  ImportInstance
newImportInstance pPlatform_ =
  ImportInstance'
    { diskImages = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      description = Prelude.Nothing,
      launchSpecification = Prelude.Nothing,
      platform = pPlatform_
    }

-- | The disk image.
importInstance_diskImages :: Lens.Lens' ImportInstance (Prelude.Maybe [DiskImage])
importInstance_diskImages = Lens.lens (\ImportInstance' {diskImages} -> diskImages) (\s@ImportInstance' {} a -> s {diskImages = a} :: ImportInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importInstance_dryRun :: Lens.Lens' ImportInstance (Prelude.Maybe Prelude.Bool)
importInstance_dryRun = Lens.lens (\ImportInstance' {dryRun} -> dryRun) (\s@ImportInstance' {} a -> s {dryRun = a} :: ImportInstance)

-- | A description for the instance being imported.
importInstance_description :: Lens.Lens' ImportInstance (Prelude.Maybe Prelude.Text)
importInstance_description = Lens.lens (\ImportInstance' {description} -> description) (\s@ImportInstance' {} a -> s {description = a} :: ImportInstance)

-- | The launch specification.
importInstance_launchSpecification :: Lens.Lens' ImportInstance (Prelude.Maybe ImportInstanceLaunchSpecification)
importInstance_launchSpecification = Lens.lens (\ImportInstance' {launchSpecification} -> launchSpecification) (\s@ImportInstance' {} a -> s {launchSpecification = a} :: ImportInstance)

-- | The instance operating system.
importInstance_platform :: Lens.Lens' ImportInstance PlatformValues
importInstance_platform = Lens.lens (\ImportInstance' {platform} -> platform) (\s@ImportInstance' {} a -> s {platform = a} :: ImportInstance)

instance Prelude.AWSRequest ImportInstance where
  type Rs ImportInstance = ImportInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportInstanceResponse'
            Prelude.<$> (x Prelude..@? "conversionTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportInstance

instance Prelude.NFData ImportInstance

instance Prelude.ToHeaders ImportInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ImportInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ImportInstance where
  toQuery ImportInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ImportInstance" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "DiskImage"
              Prelude.<$> diskImages
          ),
        "DryRun" Prelude.=: dryRun,
        "Description" Prelude.=: description,
        "LaunchSpecification" Prelude.=: launchSpecification,
        "Platform" Prelude.=: platform
      ]

-- | /See:/ 'newImportInstanceResponse' smart constructor.
data ImportInstanceResponse = ImportInstanceResponse'
  { -- | Information about the conversion task.
    conversionTask :: Prelude.Maybe ConversionTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversionTask', 'importInstanceResponse_conversionTask' - Information about the conversion task.
--
-- 'httpStatus', 'importInstanceResponse_httpStatus' - The response's http status code.
newImportInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportInstanceResponse
newImportInstanceResponse pHttpStatus_ =
  ImportInstanceResponse'
    { conversionTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the conversion task.
importInstanceResponse_conversionTask :: Lens.Lens' ImportInstanceResponse (Prelude.Maybe ConversionTask)
importInstanceResponse_conversionTask = Lens.lens (\ImportInstanceResponse' {conversionTask} -> conversionTask) (\s@ImportInstanceResponse' {} a -> s {conversionTask = a} :: ImportInstanceResponse)

-- | The response's http status code.
importInstanceResponse_httpStatus :: Lens.Lens' ImportInstanceResponse Prelude.Int
importInstanceResponse_httpStatus = Lens.lens (\ImportInstanceResponse' {httpStatus} -> httpStatus) (\s@ImportInstanceResponse' {} a -> s {httpStatus = a} :: ImportInstanceResponse)

instance Prelude.NFData ImportInstanceResponse
