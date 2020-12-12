{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatus
  ( ClientVPNAuthorizationRuleStatus (..),

    -- * Smart constructor
    mkClientVPNAuthorizationRuleStatus,

    -- * Lenses
    cvarsCode,
    cvarsMessage,
  )
where

import Network.AWS.EC2.Types.ClientVPNAuthorizationRuleStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of an authorization rule.
--
-- /See:/ 'mkClientVPNAuthorizationRuleStatus' smart constructor.
data ClientVPNAuthorizationRuleStatus = ClientVPNAuthorizationRuleStatus'
  { code ::
      Lude.Maybe
        ClientVPNAuthorizationRuleStatusCode,
    message ::
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

-- | Creates a value of 'ClientVPNAuthorizationRuleStatus' with the minimum fields required to make a request.
--
-- * 'code' - The state of the authorization rule.
-- * 'message' - A message about the status of the authorization rule, if applicable.
mkClientVPNAuthorizationRuleStatus ::
  ClientVPNAuthorizationRuleStatus
mkClientVPNAuthorizationRuleStatus =
  ClientVPNAuthorizationRuleStatus'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The state of the authorization rule.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarsCode :: Lens.Lens' ClientVPNAuthorizationRuleStatus (Lude.Maybe ClientVPNAuthorizationRuleStatusCode)
cvarsCode = Lens.lens (code :: ClientVPNAuthorizationRuleStatus -> Lude.Maybe ClientVPNAuthorizationRuleStatusCode) (\s a -> s {code = a} :: ClientVPNAuthorizationRuleStatus)
{-# DEPRECATED cvarsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A message about the status of the authorization rule, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarsMessage :: Lens.Lens' ClientVPNAuthorizationRuleStatus (Lude.Maybe Lude.Text)
cvarsMessage = Lens.lens (message :: ClientVPNAuthorizationRuleStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClientVPNAuthorizationRuleStatus)
{-# DEPRECATED cvarsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML ClientVPNAuthorizationRuleStatus where
  parseXML x =
    ClientVPNAuthorizationRuleStatus'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
