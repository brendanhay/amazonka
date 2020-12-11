-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssignedPrivateIPAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssignedPrivateIPAddress
  ( AssignedPrivateIPAddress (..),

    -- * Smart constructor
    mkAssignedPrivateIPAddress,

    -- * Lenses
    apiaPrivateIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the private IP addresses assigned to a network interface.
--
-- /See:/ 'mkAssignedPrivateIPAddress' smart constructor.
newtype AssignedPrivateIPAddress = AssignedPrivateIPAddress'
  { privateIPAddress ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignedPrivateIPAddress' with the minimum fields required to make a request.
--
-- * 'privateIPAddress' - The private IP address assigned to the network interface.
mkAssignedPrivateIPAddress ::
  AssignedPrivateIPAddress
mkAssignedPrivateIPAddress =
  AssignedPrivateIPAddress' {privateIPAddress = Lude.Nothing}

-- | The private IP address assigned to the network interface.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaPrivateIPAddress :: Lens.Lens' AssignedPrivateIPAddress (Lude.Maybe Lude.Text)
apiaPrivateIPAddress = Lens.lens (privateIPAddress :: AssignedPrivateIPAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: AssignedPrivateIPAddress)
{-# DEPRECATED apiaPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

instance Lude.FromXML AssignedPrivateIPAddress where
  parseXML x =
    AssignedPrivateIPAddress'
      Lude.<$> (x Lude..@? "privateIpAddress")
