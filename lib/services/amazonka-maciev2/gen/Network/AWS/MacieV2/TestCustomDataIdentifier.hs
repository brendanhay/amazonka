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
-- Module      : Amazonka.MacieV2.TestCustomDataIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom data identifier.
module Amazonka.MacieV2.TestCustomDataIdentifier
  ( -- * Creating a Request
    TestCustomDataIdentifier (..),
    newTestCustomDataIdentifier,

    -- * Request Lenses
    testCustomDataIdentifier_keywords,
    testCustomDataIdentifier_ignoreWords,
    testCustomDataIdentifier_maximumMatchDistance,
    testCustomDataIdentifier_regex,
    testCustomDataIdentifier_sampleText,

    -- * Destructuring the Response
    TestCustomDataIdentifierResponse (..),
    newTestCustomDataIdentifierResponse,

    -- * Response Lenses
    testCustomDataIdentifierResponse_matchCount,
    testCustomDataIdentifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestCustomDataIdentifier' smart constructor.
data TestCustomDataIdentifier = TestCustomDataIdentifier'
  { -- | An array that lists specific character sequences (keywords), one of
    -- which must be within proximity (maximumMatchDistance) of the regular
    -- expression to match. The array can contain as many as 50 keywords. Each
    -- keyword can contain 3-90 UTF-8 characters. Keywords aren\'t case
    -- sensitive.
    keywords :: Prelude.Maybe [Prelude.Text],
    -- | An array that lists specific character sequences (ignore words) to
    -- exclude from the results. If the text matched by the regular expression
    -- is the same as any string in this array, Amazon Macie ignores it. The
    -- array can contain as many as 10 ignore words. Each ignore word can
    -- contain 4-90 UTF-8 characters. Ignore words are case sensitive.
    ignoreWords :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of characters that can exist between text that
    -- matches the regex pattern and the character sequences specified by the
    -- keywords array. Amazon Macie includes or excludes a result based on the
    -- proximity of a keyword to text that matches the regex pattern. The
    -- distance can be 1-300 characters. The default value is 50.
    maximumMatchDistance :: Prelude.Maybe Prelude.Int,
    -- | The regular expression (/regex/) that defines the pattern to match. The
    -- expression can contain as many as 512 characters.
    regex :: Prelude.Text,
    -- | The sample text to inspect by using the custom data identifier. The text
    -- can contain as many as 1,000 characters.
    sampleText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestCustomDataIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keywords', 'testCustomDataIdentifier_keywords' - An array that lists specific character sequences (keywords), one of
-- which must be within proximity (maximumMatchDistance) of the regular
-- expression to match. The array can contain as many as 50 keywords. Each
-- keyword can contain 3-90 UTF-8 characters. Keywords aren\'t case
-- sensitive.
--
-- 'ignoreWords', 'testCustomDataIdentifier_ignoreWords' - An array that lists specific character sequences (ignore words) to
-- exclude from the results. If the text matched by the regular expression
-- is the same as any string in this array, Amazon Macie ignores it. The
-- array can contain as many as 10 ignore words. Each ignore word can
-- contain 4-90 UTF-8 characters. Ignore words are case sensitive.
--
-- 'maximumMatchDistance', 'testCustomDataIdentifier_maximumMatchDistance' - The maximum number of characters that can exist between text that
-- matches the regex pattern and the character sequences specified by the
-- keywords array. Amazon Macie includes or excludes a result based on the
-- proximity of a keyword to text that matches the regex pattern. The
-- distance can be 1-300 characters. The default value is 50.
--
-- 'regex', 'testCustomDataIdentifier_regex' - The regular expression (/regex/) that defines the pattern to match. The
-- expression can contain as many as 512 characters.
--
-- 'sampleText', 'testCustomDataIdentifier_sampleText' - The sample text to inspect by using the custom data identifier. The text
-- can contain as many as 1,000 characters.
newTestCustomDataIdentifier ::
  -- | 'regex'
  Prelude.Text ->
  -- | 'sampleText'
  Prelude.Text ->
  TestCustomDataIdentifier
newTestCustomDataIdentifier pRegex_ pSampleText_ =
  TestCustomDataIdentifier'
    { keywords =
        Prelude.Nothing,
      ignoreWords = Prelude.Nothing,
      maximumMatchDistance = Prelude.Nothing,
      regex = pRegex_,
      sampleText = pSampleText_
    }

-- | An array that lists specific character sequences (keywords), one of
-- which must be within proximity (maximumMatchDistance) of the regular
-- expression to match. The array can contain as many as 50 keywords. Each
-- keyword can contain 3-90 UTF-8 characters. Keywords aren\'t case
-- sensitive.
testCustomDataIdentifier_keywords :: Lens.Lens' TestCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
testCustomDataIdentifier_keywords = Lens.lens (\TestCustomDataIdentifier' {keywords} -> keywords) (\s@TestCustomDataIdentifier' {} a -> s {keywords = a} :: TestCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | An array that lists specific character sequences (ignore words) to
-- exclude from the results. If the text matched by the regular expression
-- is the same as any string in this array, Amazon Macie ignores it. The
-- array can contain as many as 10 ignore words. Each ignore word can
-- contain 4-90 UTF-8 characters. Ignore words are case sensitive.
testCustomDataIdentifier_ignoreWords :: Lens.Lens' TestCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
testCustomDataIdentifier_ignoreWords = Lens.lens (\TestCustomDataIdentifier' {ignoreWords} -> ignoreWords) (\s@TestCustomDataIdentifier' {} a -> s {ignoreWords = a} :: TestCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of characters that can exist between text that
-- matches the regex pattern and the character sequences specified by the
-- keywords array. Amazon Macie includes or excludes a result based on the
-- proximity of a keyword to text that matches the regex pattern. The
-- distance can be 1-300 characters. The default value is 50.
testCustomDataIdentifier_maximumMatchDistance :: Lens.Lens' TestCustomDataIdentifier (Prelude.Maybe Prelude.Int)
testCustomDataIdentifier_maximumMatchDistance = Lens.lens (\TestCustomDataIdentifier' {maximumMatchDistance} -> maximumMatchDistance) (\s@TestCustomDataIdentifier' {} a -> s {maximumMatchDistance = a} :: TestCustomDataIdentifier)

-- | The regular expression (/regex/) that defines the pattern to match. The
-- expression can contain as many as 512 characters.
testCustomDataIdentifier_regex :: Lens.Lens' TestCustomDataIdentifier Prelude.Text
testCustomDataIdentifier_regex = Lens.lens (\TestCustomDataIdentifier' {regex} -> regex) (\s@TestCustomDataIdentifier' {} a -> s {regex = a} :: TestCustomDataIdentifier)

-- | The sample text to inspect by using the custom data identifier. The text
-- can contain as many as 1,000 characters.
testCustomDataIdentifier_sampleText :: Lens.Lens' TestCustomDataIdentifier Prelude.Text
testCustomDataIdentifier_sampleText = Lens.lens (\TestCustomDataIdentifier' {sampleText} -> sampleText) (\s@TestCustomDataIdentifier' {} a -> s {sampleText = a} :: TestCustomDataIdentifier)

instance Core.AWSRequest TestCustomDataIdentifier where
  type
    AWSResponse TestCustomDataIdentifier =
      TestCustomDataIdentifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TestCustomDataIdentifierResponse'
            Prelude.<$> (x Core..?> "matchCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestCustomDataIdentifier

instance Prelude.NFData TestCustomDataIdentifier

instance Core.ToHeaders TestCustomDataIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TestCustomDataIdentifier where
  toJSON TestCustomDataIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("keywords" Core..=) Prelude.<$> keywords,
            ("ignoreWords" Core..=) Prelude.<$> ignoreWords,
            ("maximumMatchDistance" Core..=)
              Prelude.<$> maximumMatchDistance,
            Prelude.Just ("regex" Core..= regex),
            Prelude.Just ("sampleText" Core..= sampleText)
          ]
      )

instance Core.ToPath TestCustomDataIdentifier where
  toPath =
    Prelude.const "/custom-data-identifiers/test"

instance Core.ToQuery TestCustomDataIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestCustomDataIdentifierResponse' smart constructor.
data TestCustomDataIdentifierResponse = TestCustomDataIdentifierResponse'
  { -- | The number of instances of sample text that matched the detection
    -- criteria specified in the custom data identifier.
    matchCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestCustomDataIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchCount', 'testCustomDataIdentifierResponse_matchCount' - The number of instances of sample text that matched the detection
-- criteria specified in the custom data identifier.
--
-- 'httpStatus', 'testCustomDataIdentifierResponse_httpStatus' - The response's http status code.
newTestCustomDataIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestCustomDataIdentifierResponse
newTestCustomDataIdentifierResponse pHttpStatus_ =
  TestCustomDataIdentifierResponse'
    { matchCount =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of instances of sample text that matched the detection
-- criteria specified in the custom data identifier.
testCustomDataIdentifierResponse_matchCount :: Lens.Lens' TestCustomDataIdentifierResponse (Prelude.Maybe Prelude.Int)
testCustomDataIdentifierResponse_matchCount = Lens.lens (\TestCustomDataIdentifierResponse' {matchCount} -> matchCount) (\s@TestCustomDataIdentifierResponse' {} a -> s {matchCount = a} :: TestCustomDataIdentifierResponse)

-- | The response's http status code.
testCustomDataIdentifierResponse_httpStatus :: Lens.Lens' TestCustomDataIdentifierResponse Prelude.Int
testCustomDataIdentifierResponse_httpStatus = Lens.lens (\TestCustomDataIdentifierResponse' {httpStatus} -> httpStatus) (\s@TestCustomDataIdentifierResponse' {} a -> s {httpStatus = a} :: TestCustomDataIdentifierResponse)

instance
  Prelude.NFData
    TestCustomDataIdentifierResponse
