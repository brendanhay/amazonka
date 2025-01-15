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
-- Module      : Amazonka.EC2.ImportVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import volume task using metadata from the specified disk
-- image.
--
-- This API action supports only single-volume VMs. To import multi-volume
-- VMs, use ImportImage instead. To import a disk to a snapshot, use
-- ImportSnapshot instead.
--
-- This API action is not supported by the Command Line Interface (CLI).
-- For information about using the Amazon EC2 CLI, which is deprecated, see
-- <https://awsdocs.s3.amazonaws.com/EC2/ec2-clt.pdf#importing-your-volumes-into-amazon-ebs Importing Disks to Amazon EBS>
-- in the /Amazon EC2 CLI Reference/ PDF file.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
module Amazonka.EC2.ImportVolume
  ( -- * Creating a Request
    ImportVolume (..),
    newImportVolume,

    -- * Request Lenses
    importVolume_description,
    importVolume_dryRun,
    importVolume_availabilityZone,
    importVolume_image,
    importVolume_volume,

    -- * Destructuring the Response
    ImportVolumeResponse (..),
    newImportVolumeResponse,

    -- * Response Lenses
    importVolumeResponse_conversionTask,
    importVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportVolume' smart constructor.
data ImportVolume = ImportVolume'
  { -- | A description of the volume.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone for the resulting EBS volume.
    availabilityZone :: Prelude.Text,
    -- | The disk image.
    image :: DiskImageDetail,
    -- | The volume size.
    volume :: VolumeDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importVolume_description' - A description of the volume.
--
-- 'dryRun', 'importVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'availabilityZone', 'importVolume_availabilityZone' - The Availability Zone for the resulting EBS volume.
--
-- 'image', 'importVolume_image' - The disk image.
--
-- 'volume', 'importVolume_volume' - The volume size.
newImportVolume ::
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'image'
  DiskImageDetail ->
  -- | 'volume'
  VolumeDetail ->
  ImportVolume
newImportVolume pAvailabilityZone_ pImage_ pVolume_ =
  ImportVolume'
    { description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      availabilityZone = pAvailabilityZone_,
      image = pImage_,
      volume = pVolume_
    }

-- | A description of the volume.
importVolume_description :: Lens.Lens' ImportVolume (Prelude.Maybe Prelude.Text)
importVolume_description = Lens.lens (\ImportVolume' {description} -> description) (\s@ImportVolume' {} a -> s {description = a} :: ImportVolume)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importVolume_dryRun :: Lens.Lens' ImportVolume (Prelude.Maybe Prelude.Bool)
importVolume_dryRun = Lens.lens (\ImportVolume' {dryRun} -> dryRun) (\s@ImportVolume' {} a -> s {dryRun = a} :: ImportVolume)

-- | The Availability Zone for the resulting EBS volume.
importVolume_availabilityZone :: Lens.Lens' ImportVolume Prelude.Text
importVolume_availabilityZone = Lens.lens (\ImportVolume' {availabilityZone} -> availabilityZone) (\s@ImportVolume' {} a -> s {availabilityZone = a} :: ImportVolume)

-- | The disk image.
importVolume_image :: Lens.Lens' ImportVolume DiskImageDetail
importVolume_image = Lens.lens (\ImportVolume' {image} -> image) (\s@ImportVolume' {} a -> s {image = a} :: ImportVolume)

-- | The volume size.
importVolume_volume :: Lens.Lens' ImportVolume VolumeDetail
importVolume_volume = Lens.lens (\ImportVolume' {volume} -> volume) (\s@ImportVolume' {} a -> s {volume = a} :: ImportVolume)

instance Core.AWSRequest ImportVolume where
  type AWSResponse ImportVolume = ImportVolumeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ImportVolumeResponse'
            Prelude.<$> (x Data..@? "conversionTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportVolume where
  hashWithSalt _salt ImportVolume' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` volume

instance Prelude.NFData ImportVolume where
  rnf ImportVolume' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf availabilityZone `Prelude.seq`
          Prelude.rnf image `Prelude.seq`
            Prelude.rnf volume

instance Data.ToHeaders ImportVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ImportVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportVolume where
  toQuery ImportVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ImportVolume" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "AvailabilityZone" Data.=: availabilityZone,
        "Image" Data.=: image,
        "Volume" Data.=: volume
      ]

-- | /See:/ 'newImportVolumeResponse' smart constructor.
data ImportVolumeResponse = ImportVolumeResponse'
  { -- | Information about the conversion task.
    conversionTask :: Prelude.Maybe ConversionTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conversionTask', 'importVolumeResponse_conversionTask' - Information about the conversion task.
--
-- 'httpStatus', 'importVolumeResponse_httpStatus' - The response's http status code.
newImportVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportVolumeResponse
newImportVolumeResponse pHttpStatus_ =
  ImportVolumeResponse'
    { conversionTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the conversion task.
importVolumeResponse_conversionTask :: Lens.Lens' ImportVolumeResponse (Prelude.Maybe ConversionTask)
importVolumeResponse_conversionTask = Lens.lens (\ImportVolumeResponse' {conversionTask} -> conversionTask) (\s@ImportVolumeResponse' {} a -> s {conversionTask = a} :: ImportVolumeResponse)

-- | The response's http status code.
importVolumeResponse_httpStatus :: Lens.Lens' ImportVolumeResponse Prelude.Int
importVolumeResponse_httpStatus = Lens.lens (\ImportVolumeResponse' {httpStatus} -> httpStatus) (\s@ImportVolumeResponse' {} a -> s {httpStatus = a} :: ImportVolumeResponse)

instance Prelude.NFData ImportVolumeResponse where
  rnf ImportVolumeResponse' {..} =
    Prelude.rnf conversionTask `Prelude.seq`
      Prelude.rnf httpStatus
