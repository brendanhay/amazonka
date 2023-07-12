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
-- Module      : Amazonka.EC2.Types.EbsInstanceBlockDeviceSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EbsInstanceBlockDeviceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes information used to set up an EBS volume specified in a block
-- device mapping.
--
-- /See:/ 'newEbsInstanceBlockDeviceSpecification' smart constructor.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification'
  { -- | Indicates whether the volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the EBS volume.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EbsInstanceBlockDeviceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'ebsInstanceBlockDeviceSpecification_deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- 'volumeId', 'ebsInstanceBlockDeviceSpecification_volumeId' - The ID of the EBS volume.
newEbsInstanceBlockDeviceSpecification ::
  EbsInstanceBlockDeviceSpecification
newEbsInstanceBlockDeviceSpecification =
  EbsInstanceBlockDeviceSpecification'
    { deleteOnTermination =
        Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | Indicates whether the volume is deleted on instance termination.
ebsInstanceBlockDeviceSpecification_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDeviceSpecification_deleteOnTermination = Lens.lens (\EbsInstanceBlockDeviceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDeviceSpecification)

-- | The ID of the EBS volume.
ebsInstanceBlockDeviceSpecification_volumeId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDeviceSpecification_volumeId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeId} -> volumeId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeId = a} :: EbsInstanceBlockDeviceSpecification)

instance
  Prelude.Hashable
    EbsInstanceBlockDeviceSpecification
  where
  hashWithSalt
    _salt
    EbsInstanceBlockDeviceSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` volumeId

instance
  Prelude.NFData
    EbsInstanceBlockDeviceSpecification
  where
  rnf EbsInstanceBlockDeviceSpecification' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf volumeId

instance
  Data.ToQuery
    EbsInstanceBlockDeviceSpecification
  where
  toQuery EbsInstanceBlockDeviceSpecification' {..} =
    Prelude.mconcat
      [ "DeleteOnTermination" Data.=: deleteOnTermination,
        "VolumeId" Data.=: volumeId
      ]
