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
    arAllowed,
    arAuthDecision,
    arAuthInfo,
    arDenied,
    arMissingContextValues,
  )
where

import qualified Network.AWS.IoT.Types.Allowed as Types
import qualified Network.AWS.IoT.Types.AuthDecision as Types
import qualified Network.AWS.IoT.Types.AuthInfo as Types
import qualified Network.AWS.IoT.Types.Denied as Types
import qualified Network.AWS.IoT.Types.MissingContextValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorizer result.
--
-- /See:/ 'mkAuthResult' smart constructor.
data AuthResult = AuthResult'
  { -- | The policies and statements that allowed the specified action.
    allowed :: Core.Maybe Types.Allowed,
    -- | The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
    authDecision :: Core.Maybe Types.AuthDecision,
    -- | Authorization information.
    authInfo :: Core.Maybe Types.AuthInfo,
    -- | The policies and statements that denied the specified action.
    denied :: Core.Maybe Types.Denied,
    -- | Contains any missing context values found while evaluating policy.
    missingContextValues :: Core.Maybe [Types.MissingContextValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthResult' value with any optional fields omitted.
mkAuthResult ::
  AuthResult
mkAuthResult =
  AuthResult'
    { allowed = Core.Nothing,
      authDecision = Core.Nothing,
      authInfo = Core.Nothing,
      denied = Core.Nothing,
      missingContextValues = Core.Nothing
    }

-- | The policies and statements that allowed the specified action.
--
-- /Note:/ Consider using 'allowed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAllowed :: Lens.Lens' AuthResult (Core.Maybe Types.Allowed)
arAllowed = Lens.field @"allowed"
{-# DEPRECATED arAllowed "Use generic-lens or generic-optics with 'allowed' instead." #-}

-- | The final authorization decision of this scenario. Multiple statements are taken into account when determining the authorization decision. An explicit deny statement can override multiple allow statements.
--
-- /Note:/ Consider using 'authDecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAuthDecision :: Lens.Lens' AuthResult (Core.Maybe Types.AuthDecision)
arAuthDecision = Lens.field @"authDecision"
{-# DEPRECATED arAuthDecision "Use generic-lens or generic-optics with 'authDecision' instead." #-}

-- | Authorization information.
--
-- /Note:/ Consider using 'authInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arAuthInfo :: Lens.Lens' AuthResult (Core.Maybe Types.AuthInfo)
arAuthInfo = Lens.field @"authInfo"
{-# DEPRECATED arAuthInfo "Use generic-lens or generic-optics with 'authInfo' instead." #-}

-- | The policies and statements that denied the specified action.
--
-- /Note:/ Consider using 'denied' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDenied :: Lens.Lens' AuthResult (Core.Maybe Types.Denied)
arDenied = Lens.field @"denied"
{-# DEPRECATED arDenied "Use generic-lens or generic-optics with 'denied' instead." #-}

-- | Contains any missing context values found while evaluating policy.
--
-- /Note:/ Consider using 'missingContextValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arMissingContextValues :: Lens.Lens' AuthResult (Core.Maybe [Types.MissingContextValue])
arMissingContextValues = Lens.field @"missingContextValues"
{-# DEPRECATED arMissingContextValues "Use generic-lens or generic-optics with 'missingContextValues' instead." #-}

instance Core.FromJSON AuthResult where
  parseJSON =
    Core.withObject "AuthResult" Core.$
      \x ->
        AuthResult'
          Core.<$> (x Core..:? "allowed")
          Core.<*> (x Core..:? "authDecision")
          Core.<*> (x Core..:? "authInfo")
          Core.<*> (x Core..:? "denied")
          Core.<*> (x Core..:? "missingContextValues")
