{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ByoipCidr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ByoipCidr
  ( ByoipCidr (..),

    -- * Smart constructor
    mkByoipCidr,

    -- * Lenses
    bcState,
    bcCidr,
    bcStatusMessage,
    bcDescription,
  )
where

import Network.AWS.EC2.Types.ByoipCidrState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an address range that is provisioned for use with your AWS resources through bring your own IP addresses (BYOIP).
--
-- /See:/ 'mkByoipCidr' smart constructor.
data ByoipCidr = ByoipCidr'
  { state :: Lude.Maybe ByoipCidrState,
    cidr :: Lude.Maybe Lude.Text,
    statusMessage :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ByoipCidr' with the minimum fields required to make a request.
--
-- * 'cidr' - The address range, in CIDR notation.
-- * 'description' - The description of the address range.
-- * 'state' - The state of the address pool.
-- * 'statusMessage' - Upon success, contains the ID of the address pool. Otherwise, contains an error message.
mkByoipCidr ::
  ByoipCidr
mkByoipCidr =
  ByoipCidr'
    { state = Lude.Nothing,
      cidr = Lude.Nothing,
      statusMessage = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The state of the address pool.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcState :: Lens.Lens' ByoipCidr (Lude.Maybe ByoipCidrState)
bcState = Lens.lens (state :: ByoipCidr -> Lude.Maybe ByoipCidrState) (\s a -> s {state = a} :: ByoipCidr)
{-# DEPRECATED bcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The address range, in CIDR notation.
--
-- /Note:/ Consider using 'cidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCidr :: Lens.Lens' ByoipCidr (Lude.Maybe Lude.Text)
bcCidr = Lens.lens (cidr :: ByoipCidr -> Lude.Maybe Lude.Text) (\s a -> s {cidr = a} :: ByoipCidr)
{-# DEPRECATED bcCidr "Use generic-lens or generic-optics with 'cidr' instead." #-}

-- | Upon success, contains the ID of the address pool. Otherwise, contains an error message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcStatusMessage :: Lens.Lens' ByoipCidr (Lude.Maybe Lude.Text)
bcStatusMessage = Lens.lens (statusMessage :: ByoipCidr -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ByoipCidr)
{-# DEPRECATED bcStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The description of the address range.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcDescription :: Lens.Lens' ByoipCidr (Lude.Maybe Lude.Text)
bcDescription = Lens.lens (description :: ByoipCidr -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ByoipCidr)
{-# DEPRECATED bcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ByoipCidr where
  parseXML x =
    ByoipCidr'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "cidr")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "description")
