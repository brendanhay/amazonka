{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.CognitoUserPoolConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.CognitoUserPoolConfig
  ( CognitoUserPoolConfig (..)
  -- * Smart constructor
  , mkCognitoUserPoolConfig
  -- * Lenses
  , cupcUserPoolId
  , cupcAwsRegion
  , cupcAppIdClientRegex
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'mkCognitoUserPoolConfig' smart constructor.
data CognitoUserPoolConfig = CognitoUserPoolConfig'
  { userPoolId :: Core.Text
    -- ^ The user pool ID.
  , awsRegion :: Core.Text
    -- ^ The AWS Region in which the user pool was created.
  , appIdClientRegex :: Core.Maybe Core.Text
    -- ^ A regular expression for validating the incoming Amazon Cognito user pool app client ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CognitoUserPoolConfig' value with any optional fields omitted.
mkCognitoUserPoolConfig
    :: Core.Text -- ^ 'userPoolId'
    -> Core.Text -- ^ 'awsRegion'
    -> CognitoUserPoolConfig
mkCognitoUserPoolConfig userPoolId awsRegion
  = CognitoUserPoolConfig'{userPoolId, awsRegion,
                           appIdClientRegex = Core.Nothing}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcUserPoolId :: Lens.Lens' CognitoUserPoolConfig Core.Text
cupcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE cupcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The AWS Region in which the user pool was created.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAwsRegion :: Lens.Lens' CognitoUserPoolConfig Core.Text
cupcAwsRegion = Lens.field @"awsRegion"
{-# INLINEABLE cupcAwsRegion #-}
{-# DEPRECATED awsRegion "Use generic-lens or generic-optics with 'awsRegion' instead"  #-}

-- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
--
-- /Note:/ Consider using 'appIdClientRegex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAppIdClientRegex :: Lens.Lens' CognitoUserPoolConfig (Core.Maybe Core.Text)
cupcAppIdClientRegex = Lens.field @"appIdClientRegex"
{-# INLINEABLE cupcAppIdClientRegex #-}
{-# DEPRECATED appIdClientRegex "Use generic-lens or generic-optics with 'appIdClientRegex' instead"  #-}

instance Core.FromJSON CognitoUserPoolConfig where
        toJSON CognitoUserPoolConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("userPoolId" Core..= userPoolId),
                  Core.Just ("awsRegion" Core..= awsRegion),
                  ("appIdClientRegex" Core..=) Core.<$> appIdClientRegex])

instance Core.FromJSON CognitoUserPoolConfig where
        parseJSON
          = Core.withObject "CognitoUserPoolConfig" Core.$
              \ x ->
                CognitoUserPoolConfig' Core.<$>
                  (x Core..: "userPoolId") Core.<*> x Core..: "awsRegion" Core.<*>
                    x Core..:? "appIdClientRegex"
