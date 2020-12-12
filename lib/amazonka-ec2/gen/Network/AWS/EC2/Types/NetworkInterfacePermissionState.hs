{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermissionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePermissionState
  ( NetworkInterfacePermissionState (..),

    -- * Smart constructor
    mkNetworkInterfacePermissionState,

    -- * Lenses
    nipsState,
    nipsStatusMessage,
  )
where

import Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a network interface permission.
--
-- /See:/ 'mkNetworkInterfacePermissionState' smart constructor.
data NetworkInterfacePermissionState = NetworkInterfacePermissionState'
  { state ::
      Lude.Maybe
        NetworkInterfacePermissionStateCode,
    statusMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfacePermissionState' with the minimum fields required to make a request.
--
-- * 'state' - The state of the permission.
-- * 'statusMessage' - A status message, if applicable.
mkNetworkInterfacePermissionState ::
  NetworkInterfacePermissionState
mkNetworkInterfacePermissionState =
  NetworkInterfacePermissionState'
    { state = Lude.Nothing,
      statusMessage = Lude.Nothing
    }

-- | The state of the permission.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipsState :: Lens.Lens' NetworkInterfacePermissionState (Lude.Maybe NetworkInterfacePermissionStateCode)
nipsState = Lens.lens (state :: NetworkInterfacePermissionState -> Lude.Maybe NetworkInterfacePermissionStateCode) (\s a -> s {state = a} :: NetworkInterfacePermissionState)
{-# DEPRECATED nipsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A status message, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipsStatusMessage :: Lens.Lens' NetworkInterfacePermissionState (Lude.Maybe Lude.Text)
nipsStatusMessage = Lens.lens (statusMessage :: NetworkInterfacePermissionState -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: NetworkInterfacePermissionState)
{-# DEPRECATED nipsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Lude.FromXML NetworkInterfacePermissionState where
  parseXML x =
    NetworkInterfacePermissionState'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "statusMessage")
