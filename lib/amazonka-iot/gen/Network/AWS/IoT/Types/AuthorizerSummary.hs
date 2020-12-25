{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerSummary
  ( AuthorizerSummary (..),

    -- * Smart constructor
    mkAuthorizerSummary,

    -- * Lenses
    asAuthorizerArn,
    asAuthorizerName,
  )
where

import qualified Network.AWS.IoT.Types.AuthorizerArn as Types
import qualified Network.AWS.IoT.Types.AuthorizerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorizer summary.
--
-- /See:/ 'mkAuthorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { -- | The authorizer ARN.
    authorizerArn :: Core.Maybe Types.AuthorizerArn,
    -- | The authorizer name.
    authorizerName :: Core.Maybe Types.AuthorizerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizerSummary' value with any optional fields omitted.
mkAuthorizerSummary ::
  AuthorizerSummary
mkAuthorizerSummary =
  AuthorizerSummary'
    { authorizerArn = Core.Nothing,
      authorizerName = Core.Nothing
    }

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAuthorizerArn :: Lens.Lens' AuthorizerSummary (Core.Maybe Types.AuthorizerArn)
asAuthorizerArn = Lens.field @"authorizerArn"
{-# DEPRECATED asAuthorizerArn "Use generic-lens or generic-optics with 'authorizerArn' instead." #-}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAuthorizerName :: Lens.Lens' AuthorizerSummary (Core.Maybe Types.AuthorizerName)
asAuthorizerName = Lens.field @"authorizerName"
{-# DEPRECATED asAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

instance Core.FromJSON AuthorizerSummary where
  parseJSON =
    Core.withObject "AuthorizerSummary" Core.$
      \x ->
        AuthorizerSummary'
          Core.<$> (x Core..:? "authorizerArn") Core.<*> (x Core..:? "authorizerName")
