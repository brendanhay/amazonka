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
-- Module      : Network.AWS.EC2.Types.VolumeStatusDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeStatusName
import qualified Network.AWS.Lens as Lens

-- | Describes a volume status.
--
-- /See:/ 'newVolumeStatusDetails' smart constructor.
data VolumeStatusDetails = VolumeStatusDetails'
  { -- | The intended status of the volume status.
    status :: Core.Maybe Core.Text,
    -- | The name of the volume status.
    name :: Core.Maybe VolumeStatusName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'volumeStatusDetails_status' - The intended status of the volume status.
--
-- 'name', 'volumeStatusDetails_name' - The name of the volume status.
newVolumeStatusDetails ::
  VolumeStatusDetails
newVolumeStatusDetails =
  VolumeStatusDetails'
    { status = Core.Nothing,
      name = Core.Nothing
    }

-- | The intended status of the volume status.
volumeStatusDetails_status :: Lens.Lens' VolumeStatusDetails (Core.Maybe Core.Text)
volumeStatusDetails_status = Lens.lens (\VolumeStatusDetails' {status} -> status) (\s@VolumeStatusDetails' {} a -> s {status = a} :: VolumeStatusDetails)

-- | The name of the volume status.
volumeStatusDetails_name :: Lens.Lens' VolumeStatusDetails (Core.Maybe VolumeStatusName)
volumeStatusDetails_name = Lens.lens (\VolumeStatusDetails' {name} -> name) (\s@VolumeStatusDetails' {} a -> s {name = a} :: VolumeStatusDetails)

instance Core.FromXML VolumeStatusDetails where
  parseXML x =
    VolumeStatusDetails'
      Core.<$> (x Core..@? "status") Core.<*> (x Core..@? "name")

instance Core.Hashable VolumeStatusDetails

instance Core.NFData VolumeStatusDetails
