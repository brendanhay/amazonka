-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusDetails
  ( VolumeStatusDetails (..),

    -- * Smart constructor
    mkVolumeStatusDetails,

    -- * Lenses
    vsdStatus,
    vsdName,
  )
where

import Network.AWS.EC2.Types.VolumeStatusName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a volume status.
--
-- /See:/ 'mkVolumeStatusDetails' smart constructor.
data VolumeStatusDetails = VolumeStatusDetails'
  { status ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe VolumeStatusName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeStatusDetails' with the minimum fields required to make a request.
--
-- * 'name' - The name of the volume status.
-- * 'status' - The intended status of the volume status.
mkVolumeStatusDetails ::
  VolumeStatusDetails
mkVolumeStatusDetails =
  VolumeStatusDetails' {status = Lude.Nothing, name = Lude.Nothing}

-- | The intended status of the volume status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsdStatus :: Lens.Lens' VolumeStatusDetails (Lude.Maybe Lude.Text)
vsdStatus = Lens.lens (status :: VolumeStatusDetails -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: VolumeStatusDetails)
{-# DEPRECATED vsdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the volume status.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsdName :: Lens.Lens' VolumeStatusDetails (Lude.Maybe VolumeStatusName)
vsdName = Lens.lens (name :: VolumeStatusDetails -> Lude.Maybe VolumeStatusName) (\s a -> s {name = a} :: VolumeStatusDetails)
{-# DEPRECATED vsdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML VolumeStatusDetails where
  parseXML x =
    VolumeStatusDetails'
      Lude.<$> (x Lude..@? "status") Lude.<*> (x Lude..@? "name")
