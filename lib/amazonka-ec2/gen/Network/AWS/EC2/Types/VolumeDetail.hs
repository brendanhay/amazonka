{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeDetail
  ( VolumeDetail (..),

    -- * Smart constructor
    mkVolumeDetail,

    -- * Lenses
    vdSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EBS volume.
--
-- /See:/ 'mkVolumeDetail' smart constructor.
newtype VolumeDetail = VolumeDetail' {size :: Lude.Integer}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeDetail' with the minimum fields required to make a request.
--
-- * 'size' - The size of the volume, in GiB.
mkVolumeDetail ::
  -- | 'size'
  Lude.Integer ->
  VolumeDetail
mkVolumeDetail pSize_ = VolumeDetail' {size = pSize_}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdSize :: Lens.Lens' VolumeDetail Lude.Integer
vdSize = Lens.lens (size :: VolumeDetail -> Lude.Integer) (\s a -> s {size = a} :: VolumeDetail)
{-# DEPRECATED vdSize "Use generic-lens or generic-optics with 'size' instead." #-}

instance Lude.ToQuery VolumeDetail where
  toQuery VolumeDetail' {..} = Lude.mconcat ["Size" Lude.=: size]
