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
-- Module      : Amazonka.EC2.Types.EbsInstanceBlockDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EbsInstanceBlockDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttachmentStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a parameter used to set up an EBS volume in a block device
-- mapping.
--
-- /See:/ 'newEbsInstanceBlockDevice' smart constructor.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice'
  { -- | The time stamp when the attachment initiated.
    attachTime :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether the volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus,
    -- | The ID of the EBS volume.
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
-- 'attachTime', 'ebsInstanceBlockDevice_attachTime' - The time stamp when the attachment initiated.
--
-- 'deleteOnTermination', 'ebsInstanceBlockDevice_deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- 'status', 'ebsInstanceBlockDevice_status' - The attachment state.
--
-- 'volumeId', 'ebsInstanceBlockDevice_volumeId' - The ID of the EBS volume.
newEbsInstanceBlockDevice ::
  EbsInstanceBlockDevice
newEbsInstanceBlockDevice =
  EbsInstanceBlockDevice'
    { attachTime =
        Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      status = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | The time stamp when the attachment initiated.
ebsInstanceBlockDevice_attachTime :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe Prelude.UTCTime)
ebsInstanceBlockDevice_attachTime = Lens.lens (\EbsInstanceBlockDevice' {attachTime} -> attachTime) (\s@EbsInstanceBlockDevice' {} a -> s {attachTime = a} :: EbsInstanceBlockDevice) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the volume is deleted on instance termination.
ebsInstanceBlockDevice_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDevice_deleteOnTermination = Lens.lens (\EbsInstanceBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDevice' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDevice)

-- | The attachment state.
ebsInstanceBlockDevice_status :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe AttachmentStatus)
ebsInstanceBlockDevice_status = Lens.lens (\EbsInstanceBlockDevice' {status} -> status) (\s@EbsInstanceBlockDevice' {} a -> s {status = a} :: EbsInstanceBlockDevice)

-- | The ID of the EBS volume.
ebsInstanceBlockDevice_volumeId :: Lens.Lens' EbsInstanceBlockDevice (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDevice_volumeId = Lens.lens (\EbsInstanceBlockDevice' {volumeId} -> volumeId) (\s@EbsInstanceBlockDevice' {} a -> s {volumeId = a} :: EbsInstanceBlockDevice)

instance Data.FromXML EbsInstanceBlockDevice where
  parseXML x =
    EbsInstanceBlockDevice'
      Prelude.<$> (x Data..@? "attachTime")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "volumeId")

instance Prelude.Hashable EbsInstanceBlockDevice where
  hashWithSalt _salt EbsInstanceBlockDevice' {..} =
    _salt `Prelude.hashWithSalt` attachTime
      `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData EbsInstanceBlockDevice where
  rnf EbsInstanceBlockDevice' {..} =
    Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf volumeId
