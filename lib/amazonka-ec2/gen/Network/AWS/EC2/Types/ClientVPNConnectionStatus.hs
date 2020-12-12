{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNConnectionStatus
  ( ClientVPNConnectionStatus (..),

    -- * Smart constructor
    mkClientVPNConnectionStatus,

    -- * Lenses
    cvcsCode,
    cvcsMessage,
  )
where

import Network.AWS.EC2.Types.ClientVPNConnectionStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of a client connection.
--
-- /See:/ 'mkClientVPNConnectionStatus' smart constructor.
data ClientVPNConnectionStatus = ClientVPNConnectionStatus'
  { code ::
      Lude.Maybe
        ClientVPNConnectionStatusCode,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNConnectionStatus' with the minimum fields required to make a request.
--
-- * 'code' - The state of the client connection.
-- * 'message' - A message about the status of the client connection, if applicable.
mkClientVPNConnectionStatus ::
  ClientVPNConnectionStatus
mkClientVPNConnectionStatus =
  ClientVPNConnectionStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state of the client connection.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcsCode :: Lens.Lens' ClientVPNConnectionStatus (Lude.Maybe ClientVPNConnectionStatusCode)
cvcsCode = Lens.lens (code :: ClientVPNConnectionStatus -> Lude.Maybe ClientVPNConnectionStatusCode) (\s a -> s {code = a} :: ClientVPNConnectionStatus)
{-# DEPRECATED cvcsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message about the status of the client connection, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcsMessage :: Lens.Lens' ClientVPNConnectionStatus (Lude.Maybe Lude.Text)
cvcsMessage = Lens.lens (message :: ClientVPNConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClientVPNConnectionStatus)
{-# DEPRECATED cvcsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ClientVPNConnectionStatus where
  parseXML x =
    ClientVPNConnectionStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
