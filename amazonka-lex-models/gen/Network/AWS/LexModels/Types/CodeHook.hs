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
-- Module      : Network.AWS.LexModels.Types.CodeHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.CodeHook where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON CodeHook where
  parseJSON =
    Prelude.withObject
      "CodeHook"
      ( \x ->
          CodeHook'
            Prelude.<$> (x Prelude..: "uri")
            Prelude.<*> (x Prelude..: "messageVersion")
      )

instance Prelude.Hashable CodeHook

instance Prelude.NFData CodeHook

instance Prelude.ToJSON CodeHook where
  toJSON CodeHook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("uri" Prelude..= uri),
            Prelude.Just
              ("messageVersion" Prelude..= messageVersion)
          ]
      )
