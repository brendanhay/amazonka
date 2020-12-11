-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
  ( CustomEmailLambdaVersionConfigType (..),

    -- * Smart constructor
    mkCustomEmailLambdaVersionConfigType,

    -- * Lenses
    celvctLambdaVersion,
    celvctLambdaARN,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A custom email sender Lambda configuration type.
--
-- /See:/ 'mkCustomEmailLambdaVersionConfigType' smart constructor.
data CustomEmailLambdaVersionConfigType = CustomEmailLambdaVersionConfigType'
  { lambdaVersion ::
      CustomEmailSenderLambdaVersionType,
    lambdaARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomEmailLambdaVersionConfigType' with the minimum fields required to make a request.
--
-- * 'lambdaARN' - The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send email notifications to users.
-- * 'lambdaVersion' - The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom email Lambda function. The only supported value is @V1_0@ .
mkCustomEmailLambdaVersionConfigType ::
  -- | 'lambdaVersion'
  CustomEmailSenderLambdaVersionType ->
  -- | 'lambdaARN'
  Lude.Text ->
  CustomEmailLambdaVersionConfigType
mkCustomEmailLambdaVersionConfigType pLambdaVersion_ pLambdaARN_ =
  CustomEmailLambdaVersionConfigType'
    { lambdaVersion =
        pLambdaVersion_,
      lambdaARN = pLambdaARN_
    }

-- | The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom email Lambda function. The only supported value is @V1_0@ .
--
-- /Note:/ Consider using 'lambdaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
celvctLambdaVersion :: Lens.Lens' CustomEmailLambdaVersionConfigType CustomEmailSenderLambdaVersionType
celvctLambdaVersion = Lens.lens (lambdaVersion :: CustomEmailLambdaVersionConfigType -> CustomEmailSenderLambdaVersionType) (\s a -> s {lambdaVersion = a} :: CustomEmailLambdaVersionConfigType)
{-# DEPRECATED celvctLambdaVersion "Use generic-lens or generic-optics with 'lambdaVersion' instead." #-}

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send email notifications to users.
--
-- /Note:/ Consider using 'lambdaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
celvctLambdaARN :: Lens.Lens' CustomEmailLambdaVersionConfigType Lude.Text
celvctLambdaARN = Lens.lens (lambdaARN :: CustomEmailLambdaVersionConfigType -> Lude.Text) (\s a -> s {lambdaARN = a} :: CustomEmailLambdaVersionConfigType)
{-# DEPRECATED celvctLambdaARN "Use generic-lens or generic-optics with 'lambdaARN' instead." #-}

instance Lude.FromJSON CustomEmailLambdaVersionConfigType where
  parseJSON =
    Lude.withObject
      "CustomEmailLambdaVersionConfigType"
      ( \x ->
          CustomEmailLambdaVersionConfigType'
            Lude.<$> (x Lude..: "LambdaVersion") Lude.<*> (x Lude..: "LambdaArn")
      )

instance Lude.ToJSON CustomEmailLambdaVersionConfigType where
  toJSON CustomEmailLambdaVersionConfigType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LambdaVersion" Lude..= lambdaVersion),
            Lude.Just ("LambdaArn" Lude..= lambdaARN)
          ]
      )
