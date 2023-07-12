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
-- Module      : Amazonka.LexModels.Types.CodeHook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.CodeHook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Lambda function that verifies requests to a bot or fulfills
-- the user\'s request to a bot..
--
-- /See:/ 'newCodeHook' smart constructor.
data CodeHook = CodeHook'
  { -- | The Amazon Resource Name (ARN) of the Lambda function.
    uri :: Prelude.Text,
    -- | The version of the request-response that you want Amazon Lex to use to
    -- invoke your Lambda function. For more information, see using-lambda.
    messageVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uri', 'codeHook_uri' - The Amazon Resource Name (ARN) of the Lambda function.
--
-- 'messageVersion', 'codeHook_messageVersion' - The version of the request-response that you want Amazon Lex to use to
-- invoke your Lambda function. For more information, see using-lambda.
newCodeHook ::
  -- | 'uri'
  Prelude.Text ->
  -- | 'messageVersion'
  Prelude.Text ->
  CodeHook
newCodeHook pUri_ pMessageVersion_ =
  CodeHook'
    { uri = pUri_,
      messageVersion = pMessageVersion_
    }

-- | The Amazon Resource Name (ARN) of the Lambda function.
codeHook_uri :: Lens.Lens' CodeHook Prelude.Text
codeHook_uri = Lens.lens (\CodeHook' {uri} -> uri) (\s@CodeHook' {} a -> s {uri = a} :: CodeHook)

-- | The version of the request-response that you want Amazon Lex to use to
-- invoke your Lambda function. For more information, see using-lambda.
codeHook_messageVersion :: Lens.Lens' CodeHook Prelude.Text
codeHook_messageVersion = Lens.lens (\CodeHook' {messageVersion} -> messageVersion) (\s@CodeHook' {} a -> s {messageVersion = a} :: CodeHook)

instance Data.FromJSON CodeHook where
  parseJSON =
    Data.withObject
      "CodeHook"
      ( \x ->
          CodeHook'
            Prelude.<$> (x Data..: "uri")
            Prelude.<*> (x Data..: "messageVersion")
      )

instance Prelude.Hashable CodeHook where
  hashWithSalt _salt CodeHook' {..} =
    _salt
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` messageVersion

instance Prelude.NFData CodeHook where
  rnf CodeHook' {..} =
    Prelude.rnf uri
      `Prelude.seq` Prelude.rnf messageVersion

instance Data.ToJSON CodeHook where
  toJSON CodeHook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("uri" Data..= uri),
            Prelude.Just
              ("messageVersion" Data..= messageVersion)
          ]
      )
