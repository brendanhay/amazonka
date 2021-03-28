{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.MethodSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.MethodSnapshot
  ( MethodSnapshot (..)
  -- * Smart constructor
  , mkMethodSnapshot
  -- * Lenses
  , msApiKeyRequired
  , msAuthorizationType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a summary of a 'Method' resource, given a particular date and time.
--
-- /See:/ 'mkMethodSnapshot' smart constructor.
data MethodSnapshot = MethodSnapshot'
  { apiKeyRequired :: Core.Maybe Core.Bool
    -- ^ Specifies whether the method requires a valid 'ApiKey' .
  , authorizationType :: Core.Maybe Core.Text
    -- ^ The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MethodSnapshot' value with any optional fields omitted.
mkMethodSnapshot
    :: MethodSnapshot
mkMethodSnapshot
  = MethodSnapshot'{apiKeyRequired = Core.Nothing,
                    authorizationType = Core.Nothing}

-- | Specifies whether the method requires a valid 'ApiKey' .
--
-- /Note:/ Consider using 'apiKeyRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msApiKeyRequired :: Lens.Lens' MethodSnapshot (Core.Maybe Core.Bool)
msApiKeyRequired = Lens.field @"apiKeyRequired"
{-# INLINEABLE msApiKeyRequired #-}
{-# DEPRECATED apiKeyRequired "Use generic-lens or generic-optics with 'apiKeyRequired' instead"  #-}

-- | The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
--
-- /Note:/ Consider using 'authorizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAuthorizationType :: Lens.Lens' MethodSnapshot (Core.Maybe Core.Text)
msAuthorizationType = Lens.field @"authorizationType"
{-# INLINEABLE msAuthorizationType #-}
{-# DEPRECATED authorizationType "Use generic-lens or generic-optics with 'authorizationType' instead"  #-}

instance Core.FromJSON MethodSnapshot where
        parseJSON
          = Core.withObject "MethodSnapshot" Core.$
              \ x ->
                MethodSnapshot' Core.<$>
                  (x Core..:? "apiKeyRequired") Core.<*>
                    x Core..:? "authorizationType"
