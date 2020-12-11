-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCCidrBlockState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCCidrBlockState
  ( VPCCidrBlockState (..),

    -- * Smart constructor
    mkVPCCidrBlockState,

    -- * Lenses
    vcbsState,
    vcbsStatusMessage,
  )
where

import Network.AWS.EC2.Types.VPCCidrBlockStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a CIDR block.
--
-- /See:/ 'mkVPCCidrBlockState' smart constructor.
data VPCCidrBlockState = VPCCidrBlockState'
  { state ::
      Lude.Maybe VPCCidrBlockStateCode,
    statusMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCCidrBlockState' with the minimum fields required to make a request.
--
-- * 'state' - The state of the CIDR block.
-- * 'statusMessage' - A message about the status of the CIDR block, if applicable.
mkVPCCidrBlockState ::
  VPCCidrBlockState
mkVPCCidrBlockState =
  VPCCidrBlockState'
    { state = Lude.Nothing,
      statusMessage = Lude.Nothing
    }

-- | The state of the CIDR block.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbsState :: Lens.Lens' VPCCidrBlockState (Lude.Maybe VPCCidrBlockStateCode)
vcbsState = Lens.lens (state :: VPCCidrBlockState -> Lude.Maybe VPCCidrBlockStateCode) (\s a -> s {state = a} :: VPCCidrBlockState)
{-# DEPRECATED vcbsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A message about the status of the CIDR block, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcbsStatusMessage :: Lens.Lens' VPCCidrBlockState (Lude.Maybe Lude.Text)
vcbsStatusMessage = Lens.lens (statusMessage :: VPCCidrBlockState -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: VPCCidrBlockState)
{-# DEPRECATED vcbsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Lude.FromXML VPCCidrBlockState where
  parseXML x =
    VPCCidrBlockState'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "statusMessage")
