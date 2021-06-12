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
-- Module      : Network.AWS.EC2.Types.VolumeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an EBS volume.
--
-- /See:/ 'newVolumeDetail' smart constructor.
data VolumeDetail = VolumeDetail'
  { -- | The size of the volume, in GiB.
    size :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'volumeDetail_size' - The size of the volume, in GiB.
newVolumeDetail ::
  -- | 'size'
  Core.Integer ->
  VolumeDetail
newVolumeDetail pSize_ = VolumeDetail' {size = pSize_}

-- | The size of the volume, in GiB.
volumeDetail_size :: Lens.Lens' VolumeDetail Core.Integer
volumeDetail_size = Lens.lens (\VolumeDetail' {size} -> size) (\s@VolumeDetail' {} a -> s {size = a} :: VolumeDetail)

instance Core.Hashable VolumeDetail

instance Core.NFData VolumeDetail

instance Core.ToQuery VolumeDetail where
  toQuery VolumeDetail' {..} =
    Core.mconcat ["Size" Core.=: size]
