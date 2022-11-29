{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SnowDeviceManagement.Types.EbsInstanceBlockDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.EbsInstanceBlockDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SnowDeviceManagement.Types.AttachmentStatus

-- | Describes a parameter used to set up an Amazon Elastic Block Store
-- (Amazon EBS) volume in a block device mapping.
--
-- /See:/ 'newEbsInstanceBlockDevice' smart constructor.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice'
  { -- | A value that indicates whether the volume is deleted on instance
    -- termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus,
    -- | When the attachment was initiated.
    attachTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the Amazon EBS volume.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsInstanceBlockDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'ebsInstanceBlockDevice_deleteOnTermination' - A value that indicates whether the volume is deleted on instance
-- termination.
--
-- 'status', 'ebsInstanceBlockDevice_status' - The attachment state.
--
-- 'attachTime', 'ebsInstanceBlockDevice_attachTime' - When the attachment was initiated.
--
-- 'volumeId', 'ebsInstanceBlockDevice_volumeId' - The ID of the Amazon EBS volume.
newEbsInstanceBlockDevice ::
  EbsInstanceBlockDevice
newEbsInstanceBlockDevice =
  EbsInstanceBlockDevice'
    { deleteOnTermination =
        Prelude.Nothing,
      status = Prelude.Nothing,
      attachTime = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | A value that indicates whether the volume is deleted on instance
-- termination.
ebsInstanceBlockDevice_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDevice_deleteOnTermination = Lens.lens (\EbsInstanceBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDevice' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDevice)

-- | The attachment state.
ebsInstanceBlockDevice_status :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe AttachmentStatus)
ebsInstanceBlockDevice_status = Lens.lens (\EbsInstanceBlockDevice' {status} -> status) (\s@EbsInstanceBlockDevice' {} a -> s {status = a} :: EbsInstanceBlockDevice)

-- | When the attachment was initiated.
ebsInstanceBlockDevice_attachTime :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe Prelude.UTCTime)
ebsInstanceBlockDevice_attachTime = Lens.lens (\EbsInstanceBlockDevice' {attachTime} -> attachTime) (\s@EbsInstanceBlockDevice' {} a -> s {attachTime = a} :: EbsInstanceBlockDevice) Prelude.. Lens.mapping Core._Time

-- | The ID of the Amazon EBS volume.
ebsInstanceBlockDevice_volumeId :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDevice_volumeId = Lens.lens (\EbsInstanceBlockDevice' {volumeId} -> volumeId) (\s@EbsInstanceBlockDevice' {} a -> s {volumeId = a} :: EbsInstanceBlockDevice)

instance Core.FromJSON EbsInstanceBlockDevice where
  parseJSON =
    Core.withObject
      "EbsInstanceBlockDevice"
      ( \x ->
          EbsInstanceBlockDevice'
            Prelude.<$> (x Core..:? "deleteOnTermination")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "attachTime")
            Prelude.<*> (x Core..:? "volumeId")
      )

instance Prelude.Hashable EbsInstanceBlockDevice where
  hashWithSalt _salt EbsInstanceBlockDevice' {..} =
    _salt `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` attachTime
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData EbsInstanceBlockDevice where
  rnf EbsInstanceBlockDevice' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf volumeId
