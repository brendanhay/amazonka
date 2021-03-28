{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
  ( AnalyticsConfigurationType (..)
  -- * Smart constructor
  , mkAnalyticsConfigurationType
  -- * Lenses
  , actApplicationArn
  , actApplicationId
  , actExternalId
  , actRoleArn
  , actUserDataShared
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ApplicationArn as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ApplicationId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ExternalId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon Pinpoint analytics configuration for collecting metrics for a user pool.
--
-- /See:/ 'mkAnalyticsConfigurationType' smart constructor.
data AnalyticsConfigurationType = AnalyticsConfigurationType'
  { applicationArn :: Core.Maybe Types.ApplicationArn
    -- ^ The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can use the Amazon Pinpoint project for Pinpoint integration with the chosen User Pool Client. Amazon Cognito publishes events to the pinpoint project declared by the app ARN.
  , applicationId :: Core.Maybe Types.ApplicationId
    -- ^ The application ID for an Amazon Pinpoint application.
  , externalId :: Core.Maybe Types.ExternalId
    -- ^ The external ID.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
  , userDataShared :: Core.Maybe Core.Bool
    -- ^ If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnalyticsConfigurationType' value with any optional fields omitted.
mkAnalyticsConfigurationType
    :: AnalyticsConfigurationType
mkAnalyticsConfigurationType
  = AnalyticsConfigurationType'{applicationArn = Core.Nothing,
                                applicationId = Core.Nothing, externalId = Core.Nothing,
                                roleArn = Core.Nothing, userDataShared = Core.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon Pinpoint project. You can use the Amazon Pinpoint project for Pinpoint integration with the chosen User Pool Client. Amazon Cognito publishes events to the pinpoint project declared by the app ARN.
--
-- /Note:/ Consider using 'applicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actApplicationArn :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Types.ApplicationArn)
actApplicationArn = Lens.field @"applicationArn"
{-# INLINEABLE actApplicationArn #-}
{-# DEPRECATED applicationArn "Use generic-lens or generic-optics with 'applicationArn' instead"  #-}

-- | The application ID for an Amazon Pinpoint application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actApplicationId :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Types.ApplicationId)
actApplicationId = Lens.field @"applicationId"
{-# INLINEABLE actApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The external ID.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actExternalId :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Types.ExternalId)
actExternalId = Lens.field @"externalId"
{-# INLINEABLE actExternalId #-}
{-# DEPRECATED externalId "Use generic-lens or generic-optics with 'externalId' instead"  #-}

-- | The ARN of an IAM role that authorizes Amazon Cognito to publish events to Amazon Pinpoint analytics.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actRoleArn :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Types.RoleArn)
actRoleArn = Lens.field @"roleArn"
{-# INLINEABLE actRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | If @UserDataShared@ is @true@ , Amazon Cognito will include user data in the events it publishes to Amazon Pinpoint analytics.
--
-- /Note:/ Consider using 'userDataShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actUserDataShared :: Lens.Lens' AnalyticsConfigurationType (Core.Maybe Core.Bool)
actUserDataShared = Lens.field @"userDataShared"
{-# INLINEABLE actUserDataShared #-}
{-# DEPRECATED userDataShared "Use generic-lens or generic-optics with 'userDataShared' instead"  #-}

instance Core.FromJSON AnalyticsConfigurationType where
        toJSON AnalyticsConfigurationType{..}
          = Core.object
              (Core.catMaybes
                 [("ApplicationArn" Core..=) Core.<$> applicationArn,
                  ("ApplicationId" Core..=) Core.<$> applicationId,
                  ("ExternalId" Core..=) Core.<$> externalId,
                  ("RoleArn" Core..=) Core.<$> roleArn,
                  ("UserDataShared" Core..=) Core.<$> userDataShared])

instance Core.FromJSON AnalyticsConfigurationType where
        parseJSON
          = Core.withObject "AnalyticsConfigurationType" Core.$
              \ x ->
                AnalyticsConfigurationType' Core.<$>
                  (x Core..:? "ApplicationArn") Core.<*> x Core..:? "ApplicationId"
                    Core.<*> x Core..:? "ExternalId"
                    Core.<*> x Core..:? "RoleArn"
                    Core.<*> x Core..:? "UserDataShared"
