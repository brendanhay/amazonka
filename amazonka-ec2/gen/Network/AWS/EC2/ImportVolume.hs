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
-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import volume task using metadata from the specified disk
-- image.For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/CommandLineReference/importing-your-volumes-into-amazon-ebs.html Importing Disks to Amazon EBS>.
--
-- For information about the import manifest referenced by this API action,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
module Network.AWS.EC2.ImportVolume
  ( -- * Creating a Request
    ImportVolume (..),
    newImportVolume,

    -- * Request Lenses
    importVolume_dryRun,
    importVolume_description,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportVolume' smart constructor.
data ImportVolume = ImportVolume'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A description of the volume.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'dryRun', 'importVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'description', 'importVolume_description' - A description of the volume.
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
    { dryRun = Prelude.Nothing,
      description = Prelude.Nothing,
      availabilityZone = pAvailabilityZone_,
      image = pImage_,
      volume = pVolume_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
importVolume_dryRun :: Lens.Lens' ImportVolume (Prelude.Maybe Prelude.Bool)
importVolume_dryRun = Lens.lens (\ImportVolume' {dryRun} -> dryRun) (\s@ImportVolume' {} a -> s {dryRun = a} :: ImportVolume)

-- | A description of the volume.
importVolume_description :: Lens.Lens' ImportVolume (Prelude.Maybe Prelude.Text)
importVolume_description = Lens.lens (\ImportVolume' {description} -> description) (\s@ImportVolume' {} a -> s {description = a} :: ImportVolume)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ImportVolumeResponse'
            Prelude.<$> (x Core..@? "conversionTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportVolume

instance Prelude.NFData ImportVolume

instance Core.ToHeaders ImportVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ImportVolume where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportVolume where
  toQuery ImportVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ImportVolume" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Description" Core.=: description,
        "AvailabilityZone" Core.=: availabilityZone,
        "Image" Core.=: image,
        "Volume" Core.=: volume
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

instance Prelude.NFData ImportVolumeResponse
