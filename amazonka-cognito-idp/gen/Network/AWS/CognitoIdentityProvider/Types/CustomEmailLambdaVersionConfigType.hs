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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType where

import Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A custom email sender Lambda configuration type.
--
-- /See:/ 'newCustomEmailLambdaVersionConfigType' smart constructor.
data CustomEmailLambdaVersionConfigType = CustomEmailLambdaVersionConfigType'
  { -- | The Lambda version represents the signature of the \"request\" attribute
    -- in the \"event\" information Amazon Cognito passes to your custom email
    -- Lambda function. The only supported value is @V1_0@.
    lambdaVersion :: CustomEmailSenderLambdaVersionType,
    -- | The Lambda Amazon Resource Name of the Lambda function that Amazon
    -- Cognito triggers to send email notifications to users.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomEmailLambdaVersionConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaVersion', 'customEmailLambdaVersionConfigType_lambdaVersion' - The Lambda version represents the signature of the \"request\" attribute
-- in the \"event\" information Amazon Cognito passes to your custom email
-- Lambda function. The only supported value is @V1_0@.
--
-- 'lambdaArn', 'customEmailLambdaVersionConfigType_lambdaArn' - The Lambda Amazon Resource Name of the Lambda function that Amazon
-- Cognito triggers to send email notifications to users.
newCustomEmailLambdaVersionConfigType ::
  -- | 'lambdaVersion'
  CustomEmailSenderLambdaVersionType ->
  -- | 'lambdaArn'
  Prelude.Text ->
  CustomEmailLambdaVersionConfigType
newCustomEmailLambdaVersionConfigType
  pLambdaVersion_
  pLambdaArn_ =
    CustomEmailLambdaVersionConfigType'
      { lambdaVersion =
          pLambdaVersion_,
        lambdaArn = pLambdaArn_
      }

-- | The Lambda version represents the signature of the \"request\" attribute
-- in the \"event\" information Amazon Cognito passes to your custom email
-- Lambda function. The only supported value is @V1_0@.
customEmailLambdaVersionConfigType_lambdaVersion :: Lens.Lens' CustomEmailLambdaVersionConfigType CustomEmailSenderLambdaVersionType
customEmailLambdaVersionConfigType_lambdaVersion = Lens.lens (\CustomEmailLambdaVersionConfigType' {lambdaVersion} -> lambdaVersion) (\s@CustomEmailLambdaVersionConfigType' {} a -> s {lambdaVersion = a} :: CustomEmailLambdaVersionConfigType)

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon
-- Cognito triggers to send email notifications to users.
customEmailLambdaVersionConfigType_lambdaArn :: Lens.Lens' CustomEmailLambdaVersionConfigType Prelude.Text
customEmailLambdaVersionConfigType_lambdaArn = Lens.lens (\CustomEmailLambdaVersionConfigType' {lambdaArn} -> lambdaArn) (\s@CustomEmailLambdaVersionConfigType' {} a -> s {lambdaArn = a} :: CustomEmailLambdaVersionConfigType)

instance
  Prelude.FromJSON
    CustomEmailLambdaVersionConfigType
  where
  parseJSON =
    Prelude.withObject
      "CustomEmailLambdaVersionConfigType"
      ( \x ->
          CustomEmailLambdaVersionConfigType'
            Prelude.<$> (x Prelude..: "LambdaVersion")
            Prelude.<*> (x Prelude..: "LambdaArn")
      )

instance
  Prelude.Hashable
    CustomEmailLambdaVersionConfigType

instance
  Prelude.NFData
    CustomEmailLambdaVersionConfigType

instance
  Prelude.ToJSON
    CustomEmailLambdaVersionConfigType
  where
  toJSON CustomEmailLambdaVersionConfigType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LambdaVersion" Prelude..= lambdaVersion),
            Prelude.Just ("LambdaArn" Prelude..= lambdaArn)
          ]
      )
