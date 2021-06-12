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
-- Module      : Network.AWS.EC2.Types.VolumeStatusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeStatusDetails
import Network.AWS.EC2.Types.VolumeStatusInfoStatus
import qualified Network.AWS.Lens as Lens

-- | Describes the status of a volume.
--
-- /See:/ 'newVolumeStatusInfo' smart constructor.
data VolumeStatusInfo = VolumeStatusInfo'
  { -- | The status of the volume.
    status :: Core.Maybe VolumeStatusInfoStatus,
    -- | The details of the volume status.
    details :: Core.Maybe [VolumeStatusDetails]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'volumeStatusInfo_status' - The status of the volume.
--
-- 'details', 'volumeStatusInfo_details' - The details of the volume status.
newVolumeStatusInfo ::
  VolumeStatusInfo
newVolumeStatusInfo =
  VolumeStatusInfo'
    { status = Core.Nothing,
      details = Core.Nothing
    }

-- | The status of the volume.
volumeStatusInfo_status :: Lens.Lens' VolumeStatusInfo (Core.Maybe VolumeStatusInfoStatus)
volumeStatusInfo_status = Lens.lens (\VolumeStatusInfo' {status} -> status) (\s@VolumeStatusInfo' {} a -> s {status = a} :: VolumeStatusInfo)

-- | The details of the volume status.
volumeStatusInfo_details :: Lens.Lens' VolumeStatusInfo (Core.Maybe [VolumeStatusDetails])
volumeStatusInfo_details = Lens.lens (\VolumeStatusInfo' {details} -> details) (\s@VolumeStatusInfo' {} a -> s {details = a} :: VolumeStatusInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML VolumeStatusInfo where
  parseXML x =
    VolumeStatusInfo'
      Core.<$> (x Core..@? "status")
      Core.<*> ( x Core..@? "details" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable VolumeStatusInfo

instance Core.NFData VolumeStatusInfo
