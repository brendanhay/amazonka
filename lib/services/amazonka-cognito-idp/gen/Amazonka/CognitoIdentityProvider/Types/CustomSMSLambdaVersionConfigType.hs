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
-- Module      : Amazonka.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType where

import Amazonka.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A custom SMS sender Lambda configuration type.
--
-- /See:/ 'newCustomSMSLambdaVersionConfigType' smart constructor.
data CustomSMSLambdaVersionConfigType = CustomSMSLambdaVersionConfigType'
  { -- | Signature of the \"request\" attribute in the \"event\" information that
    -- Amazon Cognito passes to your custom SMS Lambda function. The only
    -- supported value is @V1_0@.
    lambdaVersion :: CustomSMSSenderLambdaVersionType,
    -- | The Amazon Resource Name (ARN) of the Lambda function that Amazon
    -- Cognito activates to send SMS notifications to users.
    lambdaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomSMSLambdaVersionConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaVersion', 'customSMSLambdaVersionConfigType_lambdaVersion' - Signature of the \"request\" attribute in the \"event\" information that
-- Amazon Cognito passes to your custom SMS Lambda function. The only
-- supported value is @V1_0@.
--
-- 'lambdaArn', 'customSMSLambdaVersionConfigType_lambdaArn' - The Amazon Resource Name (ARN) of the Lambda function that Amazon
-- Cognito activates to send SMS notifications to users.
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

-- | Signature of the \"request\" attribute in the \"event\" information that
-- Amazon Cognito passes to your custom SMS Lambda function. The only
-- supported value is @V1_0@.
customSMSLambdaVersionConfigType_lambdaVersion :: Lens.Lens' CustomSMSLambdaVersionConfigType CustomSMSSenderLambdaVersionType
customSMSLambdaVersionConfigType_lambdaVersion = Lens.lens (\CustomSMSLambdaVersionConfigType' {lambdaVersion} -> lambdaVersion) (\s@CustomSMSLambdaVersionConfigType' {} a -> s {lambdaVersion = a} :: CustomSMSLambdaVersionConfigType)

-- | The Amazon Resource Name (ARN) of the Lambda function that Amazon
-- Cognito activates to send SMS notifications to users.
customSMSLambdaVersionConfigType_lambdaArn :: Lens.Lens' CustomSMSLambdaVersionConfigType Prelude.Text
customSMSLambdaVersionConfigType_lambdaArn = Lens.lens (\CustomSMSLambdaVersionConfigType' {lambdaArn} -> lambdaArn) (\s@CustomSMSLambdaVersionConfigType' {} a -> s {lambdaArn = a} :: CustomSMSLambdaVersionConfigType)

instance
  Core.FromJSON
    CustomSMSLambdaVersionConfigType
  where
  parseJSON =
    Core.withObject
      "CustomSMSLambdaVersionConfigType"
      ( \x ->
          CustomSMSLambdaVersionConfigType'
            Prelude.<$> (x Core..: "LambdaVersion")
            Prelude.<*> (x Core..: "LambdaArn")
      )

instance
  Prelude.Hashable
    CustomSMSLambdaVersionConfigType
  where
  hashWithSalt
    _salt
    CustomSMSLambdaVersionConfigType' {..} =
      _salt `Prelude.hashWithSalt` lambdaVersion
        `Prelude.hashWithSalt` lambdaArn

instance
  Prelude.NFData
    CustomSMSLambdaVersionConfigType
  where
  rnf CustomSMSLambdaVersionConfigType' {..} =
    Prelude.rnf lambdaVersion
      `Prelude.seq` Prelude.rnf lambdaArn

instance Core.ToJSON CustomSMSLambdaVersionConfigType where
  toJSON CustomSMSLambdaVersionConfigType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LambdaVersion" Core..= lambdaVersion),
            Prelude.Just ("LambdaArn" Core..= lambdaArn)
          ]
      )
