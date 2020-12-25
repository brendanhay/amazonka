{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
  ( CustomSMSLambdaVersionConfigType (..),

    -- * Smart constructor
    mkCustomSMSLambdaVersionConfigType,

    -- * Lenses
    csmslvctLambdaVersion,
    csmslvctLambdaArn,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.LambdaArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A custom SMS sender Lambda configuration type.
--
-- /See:/ 'mkCustomSMSLambdaVersionConfigType' smart constructor.
data CustomSMSLambdaVersionConfigType = CustomSMSLambdaVersionConfigType'
  { -- | The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom SMS Lambda function. The only supported value is @V1_0@ .
    lambdaVersion :: Types.CustomSMSSenderLambdaVersionType,
    -- | The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send SMS notifications to users.
    lambdaArn :: Types.LambdaArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomSMSLambdaVersionConfigType' value with any optional fields omitted.
mkCustomSMSLambdaVersionConfigType ::
  -- | 'lambdaVersion'
  Types.CustomSMSSenderLambdaVersionType ->
  -- | 'lambdaArn'
  Types.LambdaArn ->
  CustomSMSLambdaVersionConfigType
mkCustomSMSLambdaVersionConfigType lambdaVersion lambdaArn =
  CustomSMSLambdaVersionConfigType' {lambdaVersion, lambdaArn}

-- | The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom SMS Lambda function. The only supported value is @V1_0@ .
--
-- /Note:/ Consider using 'lambdaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmslvctLambdaVersion :: Lens.Lens' CustomSMSLambdaVersionConfigType Types.CustomSMSSenderLambdaVersionType
csmslvctLambdaVersion = Lens.field @"lambdaVersion"
{-# DEPRECATED csmslvctLambdaVersion "Use generic-lens or generic-optics with 'lambdaVersion' instead." #-}

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send SMS notifications to users.
--
-- /Note:/ Consider using 'lambdaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmslvctLambdaArn :: Lens.Lens' CustomSMSLambdaVersionConfigType Types.LambdaArn
csmslvctLambdaArn = Lens.field @"lambdaArn"
{-# DEPRECATED csmslvctLambdaArn "Use generic-lens or generic-optics with 'lambdaArn' instead." #-}

instance Core.FromJSON CustomSMSLambdaVersionConfigType where
  toJSON CustomSMSLambdaVersionConfigType {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("LambdaVersion" Core..= lambdaVersion),
            Core.Just ("LambdaArn" Core..= lambdaArn)
          ]
      )

instance Core.FromJSON CustomSMSLambdaVersionConfigType where
  parseJSON =
    Core.withObject "CustomSMSLambdaVersionConfigType" Core.$
      \x ->
        CustomSMSLambdaVersionConfigType'
          Core.<$> (x Core..: "LambdaVersion") Core.<*> (x Core..: "LambdaArn")
