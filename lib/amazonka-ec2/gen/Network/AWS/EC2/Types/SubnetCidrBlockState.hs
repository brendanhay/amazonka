{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockState
  ( SubnetCidrBlockState (..),

    -- * Smart constructor
    mkSubnetCidrBlockState,

    -- * Lenses
    scbsState,
    scbsStatusMessage,
  )
where

import Network.AWS.EC2.Types.SubnetCidrBlockStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a CIDR block.
--
-- /See:/ 'mkSubnetCidrBlockState' smart constructor.
data SubnetCidrBlockState = SubnetCidrBlockState'
  { -- | The state of a CIDR block.
    state :: Lude.Maybe SubnetCidrBlockStateCode,
    -- | A message about the status of the CIDR block, if applicable.
    statusMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubnetCidrBlockState' with the minimum fields required to make a request.
--
-- * 'state' - The state of a CIDR block.
-- * 'statusMessage' - A message about the status of the CIDR block, if applicable.
mkSubnetCidrBlockState ::
  SubnetCidrBlockState
mkSubnetCidrBlockState =
  SubnetCidrBlockState'
    { state = Lude.Nothing,
      statusMessage = Lude.Nothing
    }

-- | The state of a CIDR block.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scbsState :: Lens.Lens' SubnetCidrBlockState (Lude.Maybe SubnetCidrBlockStateCode)
scbsState = Lens.lens (state :: SubnetCidrBlockState -> Lude.Maybe SubnetCidrBlockStateCode) (\s a -> s {state = a} :: SubnetCidrBlockState)
{-# DEPRECATED scbsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A message about the status of the CIDR block, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scbsStatusMessage :: Lens.Lens' SubnetCidrBlockState (Lude.Maybe Lude.Text)
scbsStatusMessage = Lens.lens (statusMessage :: SubnetCidrBlockState -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: SubnetCidrBlockState)
{-# DEPRECATED scbsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Lude.FromXML SubnetCidrBlockState where
  parseXML x =
    SubnetCidrBlockState'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "statusMessage")
