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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes information used to set up an EBS volume specified in a block
-- device mapping.
--
-- /See:/ 'newEbsInstanceBlockDeviceSpecification' smart constructor.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification'
  { -- | The ID of the EBS volume.
    volumeId :: Core.Maybe Core.Text,
    -- | Indicates whether the volume is deleted on instance termination.
    deleteOnTermination :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      deleteOnTermination = Core.Nothing
    }

-- | The ID of the EBS volume.
ebsInstanceBlockDeviceSpecification_volumeId :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Core.Maybe Core.Text)
ebsInstanceBlockDeviceSpecification_volumeId = Lens.lens (\EbsInstanceBlockDeviceSpecification' {volumeId} -> volumeId) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {volumeId = a} :: EbsInstanceBlockDeviceSpecification)

-- | Indicates whether the volume is deleted on instance termination.
ebsInstanceBlockDeviceSpecification_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDeviceSpecification (Core.Maybe Core.Bool)
ebsInstanceBlockDeviceSpecification_deleteOnTermination = Lens.lens (\EbsInstanceBlockDeviceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDeviceSpecification' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDeviceSpecification)

instance
  Core.Hashable
    EbsInstanceBlockDeviceSpecification

instance
  Core.NFData
    EbsInstanceBlockDeviceSpecification

instance
  Core.ToQuery
    EbsInstanceBlockDeviceSpecification
  where
  toQuery EbsInstanceBlockDeviceSpecification' {..} =
    Core.mconcat
      [ "VolumeId" Core.=: volumeId,
        "DeleteOnTermination" Core.=: deleteOnTermination
      ]
