{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MovingAddressStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MovingAddressStatus
  ( MovingAddressStatus (..),

    -- * Smart constructor
    mkMovingAddressStatus,

    -- * Lenses
    masMoveStatus,
    masPublicIP,
  )
where

import Network.AWS.EC2.Types.MoveStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of a moving Elastic IP address.
--
-- /See:/ 'mkMovingAddressStatus' smart constructor.
data MovingAddressStatus = MovingAddressStatus'
  { moveStatus ::
      Lude.Maybe MoveStatus,
    publicIP :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MovingAddressStatus' with the minimum fields required to make a request.
--
-- * 'moveStatus' - The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
-- * 'publicIP' - The Elastic IP address.
mkMovingAddressStatus ::
  MovingAddressStatus
mkMovingAddressStatus =
  MovingAddressStatus'
    { moveStatus = Lude.Nothing,
      publicIP = Lude.Nothing
    }

-- | The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
--
-- /Note:/ Consider using 'moveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masMoveStatus :: Lens.Lens' MovingAddressStatus (Lude.Maybe MoveStatus)
masMoveStatus = Lens.lens (moveStatus :: MovingAddressStatus -> Lude.Maybe MoveStatus) (\s a -> s {moveStatus = a} :: MovingAddressStatus)
{-# DEPRECATED masMoveStatus "Use generic-lens or generic-optics with 'moveStatus' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masPublicIP :: Lens.Lens' MovingAddressStatus (Lude.Maybe Lude.Text)
masPublicIP = Lens.lens (publicIP :: MovingAddressStatus -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: MovingAddressStatus)
{-# DEPRECATED masPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

instance Lude.FromXML MovingAddressStatus where
  parseXML x =
    MovingAddressStatus'
      Lude.<$> (x Lude..@? "moveStatus") Lude.<*> (x Lude..@? "publicIp")
