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
-- Module      : Amazonka.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType where

import Amazonka.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom email sender Lambda configuration type.
--
-- /See:/ 'newCustomEmailLambdaVersionConfigType' smart constructor.
data CustomEmailLambdaVersionConfigType = CustomEmailLambdaVersionConfigType'
  { -- | Signature of the \"request\" attribute in the \"event\" information
    -- Amazon Cognito passes to your custom email Lambda function. The only
    -- supported value is @V1_0@.
    lambdaVersion :: CustomEmailSenderLambdaVersionType,
    -- | The Amazon Resource Name (ARN) of the Lambda function that Amazon
    -- Cognito activates to send email notifications to users.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomEmailLambdaVersionConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaVersion', 'customEmailLambdaVersionConfigType_lambdaVersion' - Signature of the \"request\" attribute in the \"event\" information
-- Amazon Cognito passes to your custom email Lambda function. The only
-- supported value is @V1_0@.
--
-- 'lambdaArn', 'customEmailLambdaVersionConfigType_lambdaArn' - The Amazon Resource Name (ARN) of the Lambda function that Amazon
-- Cognito activates to send email notifications to users.
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

-- | Signature of the \"request\" attribute in the \"event\" information
-- Amazon Cognito passes to your custom email Lambda function. The only
-- supported value is @V1_0@.
customEmailLambdaVersionConfigType_lambdaVersion :: Lens.Lens' CustomEmailLambdaVersionConfigType CustomEmailSenderLambdaVersionType
customEmailLambdaVersionConfigType_lambdaVersion = Lens.lens (\CustomEmailLambdaVersionConfigType' {lambdaVersion} -> lambdaVersion) (\s@CustomEmailLambdaVersionConfigType' {} a -> s {lambdaVersion = a} :: CustomEmailLambdaVersionConfigType)

-- | The Amazon Resource Name (ARN) of the Lambda function that Amazon
-- Cognito activates to send email notifications to users.
customEmailLambdaVersionConfigType_lambdaArn :: Lens.Lens' CustomEmailLambdaVersionConfigType Prelude.Text
customEmailLambdaVersionConfigType_lambdaArn = Lens.lens (\CustomEmailLambdaVersionConfigType' {lambdaArn} -> lambdaArn) (\s@CustomEmailLambdaVersionConfigType' {} a -> s {lambdaArn = a} :: CustomEmailLambdaVersionConfigType)

instance
  Data.FromJSON
    CustomEmailLambdaVersionConfigType
  where
  parseJSON =
    Data.withObject
      "CustomEmailLambdaVersionConfigType"
      ( \x ->
          CustomEmailLambdaVersionConfigType'
            Prelude.<$> (x Data..: "LambdaVersion")
            Prelude.<*> (x Data..: "LambdaArn")
      )

instance
  Prelude.Hashable
    CustomEmailLambdaVersionConfigType
  where
  hashWithSalt
    _salt
    CustomEmailLambdaVersionConfigType' {..} =
      _salt `Prelude.hashWithSalt` lambdaVersion
        `Prelude.hashWithSalt` lambdaArn

instance
  Prelude.NFData
    CustomEmailLambdaVersionConfigType
  where
  rnf CustomEmailLambdaVersionConfigType' {..} =
    Prelude.rnf lambdaVersion
      `Prelude.seq` Prelude.rnf lambdaArn

instance
  Data.ToJSON
    CustomEmailLambdaVersionConfigType
  where
  toJSON CustomEmailLambdaVersionConfigType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LambdaVersion" Data..= lambdaVersion),
            Prelude.Just ("LambdaArn" Data..= lambdaArn)
          ]
      )
