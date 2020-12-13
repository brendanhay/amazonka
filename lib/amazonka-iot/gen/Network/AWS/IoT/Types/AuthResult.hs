{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthResult
  ( AuthResult (..),

    -- * Smart constructor
    mkAuthResult,

    -- * Lenses
    arDenied,
    arAuthDecision,
    arAllowed,
    arMissingContextValues,
    arAuthInfo,
  )
where

import Network.AWS.IoT.Types.Allowed
import Network.AWS.IoT.Types.AuthDecision
import Network.AWS.IoT.Types.AuthInfo
import Network.AWS.IoT.Types.Denied
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorizer result.
--
-- /See:/ 'mkAuthResult' smart constructor.
data AuthResult = AuthResult'
  { -- | The policies and statements that denied the specified action.
    denied :: Lude.Maybe Denied,
    -- | The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
    authDecision :: Lude.Maybe AuthDecision,
    -- | The policies and statements that allowed the specified action.
    allowed :: Lude.Maybe Allowed,
    -- | Contains any missing context values found while evaluating policy.
    missingContextValues :: Lude.Maybe [Lude.Text],
    -- | Authorization information.
    authInfo :: Lude.Maybe AuthInfo
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthResult' with the minimum fields required to make a request.
--
-- * 'denied' - The policies and statements that denied the specified action.
-- * 'authDecision' - The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
-- * 'allowed' - The policies and statements that allowed the specified action.
-- * 'missingContextValues' - Contains any missing context values found while evaluating policy.
-- * 'authInfo' - Authorization information.
mkAuthResult ::
  AuthResult
mkAuthResult =
  AuthResult'
    { denied = Lude.Nothing,
      authDecision = Lude.Nothing,
      allowed = Lude.Nothing,
      missingContextValues = Lude.Nothing,
      authInfo = Lude.Nothing
    }

-- | The policies and statements that denied the specified action.
--
-- /Note:/ Consider using 'denied' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDenied :: Lens.Lens' AuthResult (Lude.Maybe Denied)
arDenied = Lens.lens (denied :: AuthResult -> Lude.Maybe Denied) (\s a -> s {denied = a} :: AuthResult)
{-# DEPRECATED arDenied "Use generic-lens or generic-optics with 'denied' instead." #-}

-- | The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
--
-- /Note:/ Consider using 'authDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAuthDecision :: Lens.Lens' AuthResult (Lude.Maybe AuthDecision)
arAuthDecision = Lens.lens (authDecision :: AuthResult -> Lude.Maybe AuthDecision) (\s a -> s {authDecision = a} :: AuthResult)
{-# DEPRECATED arAuthDecision "Use generic-lens or generic-optics with 'authDecision' instead." #-}

-- | The policies and statements that allowed the specified action.
--
-- /Note:/ Consider using 'allowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAllowed :: Lens.Lens' AuthResult (Lude.Maybe Allowed)
arAllowed = Lens.lens (allowed :: AuthResult -> Lude.Maybe Allowed) (\s a -> s {allowed = a} :: AuthResult)
{-# DEPRECATED arAllowed "Use generic-lens or generic-optics with 'allowed' instead." #-}

-- | Contains any missing context values found while evaluating policy.
--
-- /Note:/ Consider using 'missingContextValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arMissingContextValues :: Lens.Lens' AuthResult (Lude.Maybe [Lude.Text])
arMissingContextValues = Lens.lens (missingContextValues :: AuthResult -> Lude.Maybe [Lude.Text]) (\s a -> s {missingContextValues = a} :: AuthResult)
{-# DEPRECATED arMissingContextValues "Use generic-lens or generic-optics with 'missingContextValues' instead." #-}

-- | Authorization information.
--
-- /Note:/ Consider using 'authInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAuthInfo :: Lens.Lens' AuthResult (Lude.Maybe AuthInfo)
arAuthInfo = Lens.lens (authInfo :: AuthResult -> Lude.Maybe AuthInfo) (\s a -> s {authInfo = a} :: AuthResult)
{-# DEPRECATED arAuthInfo "Use generic-lens or generic-optics with 'authInfo' instead." #-}

instance Lude.FromJSON AuthResult where
  parseJSON =
    Lude.withObject
      "AuthResult"
      ( \x ->
          AuthResult'
            Lude.<$> (x Lude..:? "denied")
            Lude.<*> (x Lude..:? "authDecision")
            Lude.<*> (x Lude..:? "allowed")
            Lude.<*> (x Lude..:? "missingContextValues" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "authInfo")
      )
