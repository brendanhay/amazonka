{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType where

import Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A custom SMS sender Lambda configuration type.
--
-- /See:/ 'newCustomSMSLambdaVersionConfigType' smart constructor.
data CustomSMSLambdaVersionConfigType = CustomSMSLambdaVersionConfigType'
  { -- | The Lambda version represents the signature of the \"request\" attribute
    -- in the \"event\" information Amazon Cognito passes to your custom SMS
    -- Lambda function. The only supported value is @V1_0@.
    lambdaVersion :: CustomSMSSenderLambdaVersionType,
    -- | The Lambda Amazon Resource Name of the Lambda function that Amazon
    -- Cognito triggers to send SMS notifications to users.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomSMSLambdaVersionConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaVersion', 'customSMSLambdaVersionConfigType_lambdaVersion' - The Lambda version represents the signature of the \"request\" attribute
-- in the \"event\" information Amazon Cognito passes to your custom SMS
-- Lambda function. The only supported value is @V1_0@.
--
-- 'lambdaArn', 'customSMSLambdaVersionConfigType_lambdaArn' - The Lambda Amazon Resource Name of the Lambda function that Amazon
-- Cognito triggers to send SMS notifications to users.
newCustomSMSLambdaVersionConfigType ::
  -- | 'lambdaVersion'
  CustomSMSSenderLambdaVersionType ->
  -- | 'lambdaArn'
  Prelude.Text ->
  CustomSMSLambdaVersionConfigType
newCustomSMSLambdaVersionConfigType
  pLambdaVersion_
  pLambdaArn_ =
    CustomSMSLambdaVersionConfigType'
      { lambdaVersion =
          pLambdaVersion_,
        lambdaArn = pLambdaArn_
      }

-- | The Lambda version represents the signature of the \"request\" attribute
-- in the \"event\" information Amazon Cognito passes to your custom SMS
-- Lambda function. The only supported value is @V1_0@.
customSMSLambdaVersionConfigType_lambdaVersion :: Lens.Lens' CustomSMSLambdaVersionConfigType CustomSMSSenderLambdaVersionType
customSMSLambdaVersionConfigType_lambdaVersion = Lens.lens (\CustomSMSLambdaVersionConfigType' {lambdaVersion} -> lambdaVersion) (\s@CustomSMSLambdaVersionConfigType' {} a -> s {lambdaVersion = a} :: CustomSMSLambdaVersionConfigType)

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon
-- Cognito triggers to send SMS notifications to users.
customSMSLambdaVersionConfigType_lambdaArn :: Lens.Lens' CustomSMSLambdaVersionConfigType Prelude.Text
customSMSLambdaVersionConfigType_lambdaArn = Lens.lens (\CustomSMSLambdaVersionConfigType' {lambdaArn} -> lambdaArn) (\s@CustomSMSLambdaVersionConfigType' {} a -> s {lambdaArn = a} :: CustomSMSLambdaVersionConfigType)

instance
  Prelude.FromJSON
    CustomSMSLambdaVersionConfigType
  where
  parseJSON =
    Prelude.withObject
      "CustomSMSLambdaVersionConfigType"
      ( \x ->
          CustomSMSLambdaVersionConfigType'
            Prelude.<$> (x Prelude..: "LambdaVersion")
            Prelude.<*> (x Prelude..: "LambdaArn")
      )

instance
  Prelude.Hashable
    CustomSMSLambdaVersionConfigType

instance
  Prelude.NFData
    CustomSMSLambdaVersionConfigType

instance
  Prelude.ToJSON
    CustomSMSLambdaVersionConfigType
  where
  toJSON CustomSMSLambdaVersionConfigType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LambdaVersion" Prelude..= lambdaVersion),
            Prelude.Just ("LambdaArn" Prelude..= lambdaArn)
          ]
      )
