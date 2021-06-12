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
-- Module      : Network.AWS.Comprehend.DetectSyntax
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for syntax and the part of speech of words in the
-- document. For more information, how-syntax.
module Network.AWS.Comprehend.DetectSyntax
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

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectSyntax' smart constructor.
data DetectSyntax = DetectSyntax'
  { -- | A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF
    -- encoded characters.
    text :: Core.Sensitive Core.Text,
    -- | The language code of the input documents. You can specify any of the
    -- following languages supported by Amazon Comprehend: German (\"de\"),
    -- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
    -- or Portuguese (\"pt\").
    languageCode :: SyntaxLanguageCode
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectSyntax' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectSyntax_text' - A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF
-- encoded characters.
--
-- 'languageCode', 'detectSyntax_languageCode' - The language code of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\").
newDetectSyntax ::
  -- | 'text'
  Core.Text ->
  -- | 'languageCode'
  SyntaxLanguageCode ->
  DetectSyntax
newDetectSyntax pText_ pLanguageCode_ =
  DetectSyntax'
    { text = Core._Sensitive Lens.# pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 string. Each string must contain fewer that 5,000 bytes of UTF
-- encoded characters.
detectSyntax_text :: Lens.Lens' DetectSyntax Core.Text
detectSyntax_text = Lens.lens (\DetectSyntax' {text} -> text) (\s@DetectSyntax' {} a -> s {text = a} :: DetectSyntax) Core.. Core._Sensitive

-- | The language code of the input documents. You can specify any of the
-- following languages supported by Amazon Comprehend: German (\"de\"),
-- English (\"en\"), Spanish (\"es\"), French (\"fr\"), Italian (\"it\"),
-- or Portuguese (\"pt\").
detectSyntax_languageCode :: Lens.Lens' DetectSyntax SyntaxLanguageCode
detectSyntax_languageCode = Lens.lens (\DetectSyntax' {languageCode} -> languageCode) (\s@DetectSyntax' {} a -> s {languageCode = a} :: DetectSyntax)

instance Core.AWSRequest DetectSyntax where
  type AWSResponse DetectSyntax = DetectSyntaxResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectSyntaxResponse'
            Core.<$> (x Core..?> "SyntaxTokens" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetectSyntax

instance Core.NFData DetectSyntax

instance Core.ToHeaders DetectSyntax where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectSyntax" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetectSyntax where
  toJSON DetectSyntax' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath DetectSyntax where
  toPath = Core.const "/"

instance Core.ToQuery DetectSyntax where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetectSyntaxResponse' smart constructor.
data DetectSyntaxResponse = DetectSyntaxResponse'
  { -- | A collection of syntax tokens describing the text. For each token, the
    -- response provides the text, the token type, where the text begins and
    -- ends, and the level of confidence that Amazon Comprehend has that the
    -- token is correct. For a list of token types, see how-syntax.
    syntaxTokens :: Core.Maybe [SyntaxToken],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- token is correct. For a list of token types, see how-syntax.
--
-- 'httpStatus', 'detectSyntaxResponse_httpStatus' - The response's http status code.
newDetectSyntaxResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetectSyntaxResponse
newDetectSyntaxResponse pHttpStatus_ =
  DetectSyntaxResponse'
    { syntaxTokens = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of syntax tokens describing the text. For each token, the
-- response provides the text, the token type, where the text begins and
-- ends, and the level of confidence that Amazon Comprehend has that the
-- token is correct. For a list of token types, see how-syntax.
detectSyntaxResponse_syntaxTokens :: Lens.Lens' DetectSyntaxResponse (Core.Maybe [SyntaxToken])
detectSyntaxResponse_syntaxTokens = Lens.lens (\DetectSyntaxResponse' {syntaxTokens} -> syntaxTokens) (\s@DetectSyntaxResponse' {} a -> s {syntaxTokens = a} :: DetectSyntaxResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectSyntaxResponse_httpStatus :: Lens.Lens' DetectSyntaxResponse Core.Int
detectSyntaxResponse_httpStatus = Lens.lens (\DetectSyntaxResponse' {httpStatus} -> httpStatus) (\s@DetectSyntaxResponse' {} a -> s {httpStatus = a} :: DetectSyntaxResponse)

instance Core.NFData DetectSyntaxResponse
