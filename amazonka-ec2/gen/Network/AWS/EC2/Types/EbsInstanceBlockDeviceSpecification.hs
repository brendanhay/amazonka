{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EbsInstanceBlockDeviceSpecification where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes information used to set up an EBS volume specified in a block
-- device mapping.
--
-- /See:/ 'newEbsInstanceBlockDeviceSpecification' smart constructor.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification'
  { -- | The ID of the EBS volume.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EbsInstanceBlockDeviceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeId', 'ebsInstanceBlockDeviceSpecification_volumeId' - The ID of the EBS volume.
--
-- 'deleteOnTermination', 'ebsInstanceBlockDeviceSpecification_deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
newEbsInstanceBlockDeviceSpecification ::
  EbsInstanceBlockDeviceSpecification
newEbsInstanceBlockDeviceSpecification =
  EbsInstanceBlockDeviceSpecification'
    { volumeId =
        Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing
    }

-- | The ID of the EBS volume.
ebsInstanceBlockDeviceSpecification_volumeId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Text)
ebsInstanceBlockDeviceSpecification_volumeId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeId} -> volumeId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeId = a} :: EbsInstanceBlockDeviceSpecification)

-- | Indicates whether the volume is deleted on instance termination.
ebsInstanceBlockDeviceSpecification_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Prelude.Maybe Prelude.Bool)
ebsInstanceBlockDeviceSpecification_deleteOnTermination = Lens.lens (\EbsInstanceBlockDeviceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDeviceSpecification)

instance
  Prelude.Hashable
    EbsInstanceBlockDeviceSpecification

instance
  Prelude.NFData
    EbsInstanceBlockDeviceSpecification

instance
  Prelude.ToQuery
    EbsInstanceBlockDeviceSpecification
  where
  toQuery EbsInstanceBlockDeviceSpecification' {..} =
    Prelude.mconcat
      [ "VolumeId" Prelude.=: volumeId,
        "DeleteOnTermination" Prelude.=: deleteOnTermination
      ]
