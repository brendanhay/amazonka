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
-- Module      : Amazonka.EC2.ImportInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import instance task using metadata from the specified disk
-- image.
--
-- This API action supports only single-volume VMs. To import multi-volume
-- VMs, use ImportImage instead.
--
-- This API action is not supported by the Command Line Interface (CLI).
-- For information about using the Amazon EC2 CLI, which is deprecated, see
-- <https://awsdocs.s3.amazonaws.com/EC2/ec2-clt.pdf#UsingVirtualMachinesinAmazonEC2 Importing a VM to Amazon EC2>
-- in the /Amazon EC2 CLI Reference/ PDF file.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
module Amazonka.EC2.ImportInstance
  ( -- * Creating a Request
    ImportInstance (..),
    newImportInstance,

    -- * Request Lenses
    importInstance_description,
    importInstance_diskImages,
    importInstance_dryRun,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportInstance' smart constructor.
data ImportInstance = ImportInstance'
  { -- | A description for the instance being imported.
    description :: Prelude.Maybe Prelude.Text,
    -- | The disk image.
    diskImages :: Prelude.Maybe [DiskImage],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The launch specification.
    launchSpecification :: Prelude.Maybe ImportInstanceLaunchSpecification,
    -- | The instance operating system.
    platform :: PlatformValues
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importInstance_description' - A description for the instance being imported.
--
-- 'diskImages', 'importInstance_diskImages' - The disk image.
--
-- 'dryRun', 'importInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
    { description = Prelude.Nothing,
      diskImages = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      launchSpecification = Prelude.Nothing,
      platform = pPlatform_
    }

-- | A description for the instance being imported.
importInstance_description :: Lens.Lens' ImportInstance (Prelude.Maybe Prelude.Text)
importInstance_description = Lens.lens (\ImportInstance' {description} -> description) (\s@ImportInstance' {} a -> s {description = a} :: ImportInstance)

-- | The disk image.
importInstance_diskImages :: Lens.Lens' ImportInstance (Prelude.Maybe [DiskImage])
importInstance_diskImages = Lens.lens (\ImportInstance' {diskImages} -> diskImages) (\s@ImportInstance' {} a -> s {diskImages = a} :: ImportInstance) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importInstance_dryRun :: Lens.Lens' ImportInstance (Prelude.Maybe Prelude.Bool)
importInstance_dryRun = Lens.lens (\ImportInstance' {dryRun} -> dryRun) (\s@ImportInstance' {} a -> s {dryRun = a} :: ImportInstance)

-- | The launch specification.
importInstance_launchSpecification :: Lens.Lens' ImportInstance (Prelude.Maybe ImportInstanceLaunchSpecification)
importInstance_launchSpecification = Lens.lens (\ImportInstance' {launchSpecification} -> launchSpecification) (\s@ImportInstance' {} a -> s {launchSpecification = a} :: ImportInstance)

-- | The instance operating system.
importInstance_platform :: Lens.Lens' ImportInstance PlatformValues
importInstance_platform = Lens.lens (\ImportInstance' {platform} -> platform) (\s@ImportInstance' {} a -> s {platform = a} :: ImportInstance)

instance Core.AWSRequest ImportInstance where
  type
    AWSResponse ImportInstance =
      ImportInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ImportInstanceResponse'
            Prelude.<$> (x Data..@? "conversionTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportInstance where
  hashWithSalt _salt ImportInstance' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` diskImages
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchSpecification
      `Prelude.hashWithSalt` platform

instance Prelude.NFData ImportInstance where
  rnf ImportInstance' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf diskImages
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchSpecification
      `Prelude.seq` Prelude.rnf platform

instance Data.ToHeaders ImportInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ImportInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportInstance where
  toQuery ImportInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ImportInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        Data.toQuery
          ( Data.toQueryList "DiskImage"
              Prelude.<$> diskImages
          ),
        "DryRun" Data.=: dryRun,
        "LaunchSpecification" Data.=: launchSpecification,
        "Platform" Data.=: platform
      ]

-- | /See:/ 'newImportInstanceResponse' smart constructor.
data ImportInstanceResponse = ImportInstanceResponse'
  { -- | Information about the conversion task.
    conversionTask :: Prelude.Maybe ConversionTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData ImportInstanceResponse where
  rnf ImportInstanceResponse' {..} =
    Prelude.rnf conversionTask
      `Prelude.seq` Prelude.rnf httpStatus
