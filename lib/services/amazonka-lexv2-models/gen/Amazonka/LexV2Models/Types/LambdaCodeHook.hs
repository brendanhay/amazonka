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
-- Module      : Amazonka.LexV2Models.Types.LambdaCodeHook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.LambdaCodeHook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Lambda function that verifies requests to a bot or fulfills
-- the user\'s request to a bot.
--
-- /See:/ 'newLambdaCodeHook' smart constructor.
data LambdaCodeHook = LambdaCodeHook'
  { -- | The Amazon Resource Name (ARN) of the Lambda function.
    lambdaARN :: Prelude.Text,
    -- | The version of the request-response that you want Amazon Lex to use to
    -- invoke your Lambda function.
    codeHookInterfaceVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaCodeHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaARN', 'lambdaCodeHook_lambdaARN' - The Amazon Resource Name (ARN) of the Lambda function.
--
-- 'codeHookInterfaceVersion', 'lambdaCodeHook_codeHookInterfaceVersion' - The version of the request-response that you want Amazon Lex to use to
-- invoke your Lambda function.
newLambdaCodeHook ::
  -- | 'lambdaARN'
  Prelude.Text ->
  -- | 'codeHookInterfaceVersion'
  Prelude.Text ->
  LambdaCodeHook
newLambdaCodeHook
  pLambdaARN_
  pCodeHookInterfaceVersion_ =
    LambdaCodeHook'
      { lambdaARN = pLambdaARN_,
        codeHookInterfaceVersion =
          pCodeHookInterfaceVersion_
      }

-- | The Amazon Resource Name (ARN) of the Lambda function.
lambdaCodeHook_lambdaARN :: Lens.Lens' LambdaCodeHook Prelude.Text
lambdaCodeHook_lambdaARN = Lens.lens (\LambdaCodeHook' {lambdaARN} -> lambdaARN) (\s@LambdaCodeHook' {} a -> s {lambdaARN = a} :: LambdaCodeHook)

-- | The version of the request-response that you want Amazon Lex to use to
-- invoke your Lambda function.
lambdaCodeHook_codeHookInterfaceVersion :: Lens.Lens' LambdaCodeHook Prelude.Text
lambdaCodeHook_codeHookInterfaceVersion = Lens.lens (\LambdaCodeHook' {codeHookInterfaceVersion} -> codeHookInterfaceVersion) (\s@LambdaCodeHook' {} a -> s {codeHookInterfaceVersion = a} :: LambdaCodeHook)

instance Data.FromJSON LambdaCodeHook where
  parseJSON =
    Data.withObject
      "LambdaCodeHook"
      ( \x ->
          LambdaCodeHook'
            Prelude.<$> (x Data..: "lambdaARN")
            Prelude.<*> (x Data..: "codeHookInterfaceVersion")
      )

instance Prelude.Hashable LambdaCodeHook where
  hashWithSalt _salt LambdaCodeHook' {..} =
    _salt
      `Prelude.hashWithSalt` lambdaARN
      `Prelude.hashWithSalt` codeHookInterfaceVersion

instance Prelude.NFData LambdaCodeHook where
  rnf LambdaCodeHook' {..} =
    Prelude.rnf lambdaARN `Prelude.seq`
      Prelude.rnf codeHookInterfaceVersion

instance Data.ToJSON LambdaCodeHook where
  toJSON LambdaCodeHook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("lambdaARN" Data..= lambdaARN),
            Prelude.Just
              ( "codeHookInterfaceVersion"
                  Data..= codeHookInterfaceVersion
              )
          ]
      )
