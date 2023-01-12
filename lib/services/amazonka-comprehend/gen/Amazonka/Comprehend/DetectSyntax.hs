{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.DetectSyntax
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for syntax and the part of speech of words in the
-- document. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-syntax.html Syntax>
-- in the Comprehend Developer Guide.
module Amazonka.Comprehend.DetectSyntax
  ( -- * Creating a Request
    DetectSyntax (..),
    newDetectSyntax,

    -- * Request Lenses
    detectSyntax_text,
    detectSyntax_languageCode,

    -- * Destructuring the Response
    DetectSyntaxResponse (..),
    newDetectSyntaxResponse,

    -- * Response Lenses
    detectSyntaxResponse_syntaxTokens,
    detectSyntaxResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectSyntax' smart constructor.
data DetectSyntax = DetectSyntax'
  { -- | A UTF-8 string. The maximum string size is 5 KB.
    text :: Data.Sensitive Prelude.Text,
    -- | The language code of the input documents. You can specify any of the
    -- following languages supported by Amazon Comprehend: German (\"de\"),
    -- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
    -- or Portuguese (\"pt\").
    languageCode :: SyntaxLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectSyntax' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectSyntax_text' - A UTF-8 string. The maximum string size is 5 KB.
--
-- 'languageCode', 'detectSyntax_languageCode' - The language code of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\").
newDetectSyntax ::
  -- | 'text'
  Prelude.Text ->
  -- | 'languageCode'
  SyntaxLanguageCode ->
  DetectSyntax
newDetectSyntax pText_ pLanguageCode_ =
  DetectSyntax'
    { text = Data._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 string. The maximum string size is 5 KB.
detectSyntax_text :: Lens.Lens' DetectSyntax Prelude.Text
detectSyntax_text = Lens.lens (\DetectSyntax' {text} -> text) (\s@DetectSyntax' {} a -> s {text = a} :: DetectSyntax) Prelude.. Data._Sensitive

-- | The language code of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\").
detectSyntax_languageCode :: Lens.Lens' DetectSyntax SyntaxLanguageCode
detectSyntax_languageCode = Lens.lens (\DetectSyntax' {languageCode} -> languageCode) (\s@DetectSyntax' {} a -> s {languageCode = a} :: DetectSyntax)

instance Core.AWSRequest DetectSyntax where
  type AWSResponse DetectSyntax = DetectSyntaxResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectSyntaxResponse'
            Prelude.<$> (x Data..?> "SyntaxTokens" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectSyntax where
  hashWithSalt _salt DetectSyntax' {..} =
    _salt `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData DetectSyntax where
  rnf DetectSyntax' {..} =
    Prelude.rnf text
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToHeaders DetectSyntax where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DetectSyntax" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectSyntax where
  toJSON DetectSyntax' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Data..= text),
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )

instance Data.ToPath DetectSyntax where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectSyntax where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectSyntaxResponse' smart constructor.
data DetectSyntaxResponse = DetectSyntaxResponse'
  { -- | A collection of syntax tokens describing the text. For each token, the
    -- response provides the text, the token type, where the text begins and
    -- ends, and the level of confidence that Amazon Comprehend has that the
    -- token is correct. For a list of token types, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/how-syntax.html Syntax>
    -- in the Comprehend Developer Guide.
    syntaxTokens :: Prelude.Maybe [SyntaxToken],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectSyntaxResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syntaxTokens', 'detectSyntaxResponse_syntaxTokens' - A collection of syntax tokens describing the text. For each token, the
-- response provides the text, the token type, where the text begins and
-- ends, and the level of confidence that Amazon Comprehend has that the
-- token is correct. For a list of token types, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-syntax.html Syntax>
-- in the Comprehend Developer Guide.
--
-- 'httpStatus', 'detectSyntaxResponse_httpStatus' - The response's http status code.
newDetectSyntaxResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectSyntaxResponse
newDetectSyntaxResponse pHttpStatus_ =
  DetectSyntaxResponse'
    { syntaxTokens =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of syntax tokens describing the text. For each token, the
-- response provides the text, the token type, where the text begins and
-- ends, and the level of confidence that Amazon Comprehend has that the
-- token is correct. For a list of token types, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-syntax.html Syntax>
-- in the Comprehend Developer Guide.
detectSyntaxResponse_syntaxTokens :: Lens.Lens' DetectSyntaxResponse (Prelude.Maybe [SyntaxToken])
detectSyntaxResponse_syntaxTokens = Lens.lens (\DetectSyntaxResponse' {syntaxTokens} -> syntaxTokens) (\s@DetectSyntaxResponse' {} a -> s {syntaxTokens = a} :: DetectSyntaxResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectSyntaxResponse_httpStatus :: Lens.Lens' DetectSyntaxResponse Prelude.Int
detectSyntaxResponse_httpStatus = Lens.lens (\DetectSyntaxResponse' {httpStatus} -> httpStatus) (\s@DetectSyntaxResponse' {} a -> s {httpStatus = a} :: DetectSyntaxResponse)

instance Prelude.NFData DetectSyntaxResponse where
  rnf DetectSyntaxResponse' {..} =
    Prelude.rnf syntaxTokens
      `Prelude.seq` Prelude.rnf httpStatus
