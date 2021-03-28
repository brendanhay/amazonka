{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
  ( CustomEmailLambdaVersionConfigType (..)
  -- * Smart constructor
  , mkCustomEmailLambdaVersionConfigType
  -- * Lenses
  , celvctLambdaVersion
  , celvctLambdaArn
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.LambdaArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A custom email sender Lambda configuration type.
--
-- /See:/ 'mkCustomEmailLambdaVersionConfigType' smart constructor.
data CustomEmailLambdaVersionConfigType = CustomEmailLambdaVersionConfigType'
  { lambdaVersion :: Types.CustomEmailSenderLambdaVersionType
    -- ^ The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom email Lambda function. The only supported value is @V1_0@ .
  , lambdaArn :: Types.LambdaArn
    -- ^ The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send email notifications to users.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomEmailLambdaVersionConfigType' value with any optional fields omitted.
mkCustomEmailLambdaVersionConfigType
    :: Types.CustomEmailSenderLambdaVersionType -- ^ 'lambdaVersion'
    -> Types.LambdaArn -- ^ 'lambdaArn'
    -> CustomEmailLambdaVersionConfigType
mkCustomEmailLambdaVersionConfigType lambdaVersion lambdaArn
  = CustomEmailLambdaVersionConfigType'{lambdaVersion, lambdaArn}

-- | The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom email Lambda function. The only supported value is @V1_0@ .
--
-- /Note:/ Consider using 'lambdaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
celvctLambdaVersion :: Lens.Lens' CustomEmailLambdaVersionConfigType Types.CustomEmailSenderLambdaVersionType
celvctLambdaVersion = Lens.field @"lambdaVersion"
{-# INLINEABLE celvctLambdaVersion #-}
{-# DEPRECATED lambdaVersion "Use generic-lens or generic-optics with 'lambdaVersion' instead"  #-}

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send email notifications to users.
--
-- /Note:/ Consider using 'lambdaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
celvctLambdaArn :: Lens.Lens' CustomEmailLambdaVersionConfigType Types.LambdaArn
celvctLambdaArn = Lens.field @"lambdaArn"
{-# INLINEABLE celvctLambdaArn #-}
{-# DEPRECATED lambdaArn "Use generic-lens or generic-optics with 'lambdaArn' instead"  #-}

instance Core.FromJSON CustomEmailLambdaVersionConfigType where
        toJSON CustomEmailLambdaVersionConfigType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LambdaVersion" Core..= lambdaVersion),
                  Core.Just ("LambdaArn" Core..= lambdaArn)])

instance Core.FromJSON CustomEmailLambdaVersionConfigType where
        parseJSON
          = Core.withObject "CustomEmailLambdaVersionConfigType" Core.$
              \ x ->
                CustomEmailLambdaVersionConfigType' Core.<$>
                  (x Core..: "LambdaVersion") Core.<*> x Core..: "LambdaArn"
