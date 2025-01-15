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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests a custom data identifier.
module Amazonka.MacieV2.TestCustomDataIdentifier
  ( -- * Creating a Request
    TestCustomDataIdentifier (..),
    newTestCustomDataIdentifier,

    -- * Request Lenses
    testCustomDataIdentifier_ignoreWords,
    testCustomDataIdentifier_keywords,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestCustomDataIdentifier' smart constructor.
data TestCustomDataIdentifier = TestCustomDataIdentifier'
  { -- | An array that lists specific character sequences (/ignore words/) to
    -- exclude from the results. If the text matched by the regular expression
    -- contains any string in this array, Amazon Macie ignores it. The array
    -- can contain as many as 10 ignore words. Each ignore word can contain
    -- 4-90 UTF-8 characters. Ignore words are case sensitive.
    ignoreWords :: Prelude.Maybe [Prelude.Text],
    -- | An array that lists specific character sequences (/keywords/), one of
    -- which must precede and be within proximity (maximumMatchDistance) of the
    -- regular expression to match. The array can contain as many as 50
    -- keywords. Each keyword can contain 3-90 UTF-8 characters. Keywords
    -- aren\'t case sensitive.
    keywords :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of characters that can exist between the end of at
    -- least one complete character sequence specified by the keywords array
    -- and the end of the text that matches the regex pattern. If a complete
    -- keyword precedes all the text that matches the pattern and the keyword
    -- is within the specified distance, Amazon Macie includes the result. The
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
-- 'ignoreWords', 'testCustomDataIdentifier_ignoreWords' - An array that lists specific character sequences (/ignore words/) to
-- exclude from the results. If the text matched by the regular expression
-- contains any string in this array, Amazon Macie ignores it. The array
-- can contain as many as 10 ignore words. Each ignore word can contain
-- 4-90 UTF-8 characters. Ignore words are case sensitive.
--
-- 'keywords', 'testCustomDataIdentifier_keywords' - An array that lists specific character sequences (/keywords/), one of
-- which must precede and be within proximity (maximumMatchDistance) of the
-- regular expression to match. The array can contain as many as 50
-- keywords. Each keyword can contain 3-90 UTF-8 characters. Keywords
-- aren\'t case sensitive.
--
-- 'maximumMatchDistance', 'testCustomDataIdentifier_maximumMatchDistance' - The maximum number of characters that can exist between the end of at
-- least one complete character sequence specified by the keywords array
-- and the end of the text that matches the regex pattern. If a complete
-- keyword precedes all the text that matches the pattern and the keyword
-- is within the specified distance, Amazon Macie includes the result. The
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
    { ignoreWords =
        Prelude.Nothing,
      keywords = Prelude.Nothing,
      maximumMatchDistance = Prelude.Nothing,
      regex = pRegex_,
      sampleText = pSampleText_
    }

-- | An array that lists specific character sequences (/ignore words/) to
-- exclude from the results. If the text matched by the regular expression
-- contains any string in this array, Amazon Macie ignores it. The array
-- can contain as many as 10 ignore words. Each ignore word can contain
-- 4-90 UTF-8 characters. Ignore words are case sensitive.
testCustomDataIdentifier_ignoreWords :: Lens.Lens' TestCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
testCustomDataIdentifier_ignoreWords = Lens.lens (\TestCustomDataIdentifier' {ignoreWords} -> ignoreWords) (\s@TestCustomDataIdentifier' {} a -> s {ignoreWords = a} :: TestCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | An array that lists specific character sequences (/keywords/), one of
-- which must precede and be within proximity (maximumMatchDistance) of the
-- regular expression to match. The array can contain as many as 50
-- keywords. Each keyword can contain 3-90 UTF-8 characters. Keywords
-- aren\'t case sensitive.
testCustomDataIdentifier_keywords :: Lens.Lens' TestCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
testCustomDataIdentifier_keywords = Lens.lens (\TestCustomDataIdentifier' {keywords} -> keywords) (\s@TestCustomDataIdentifier' {} a -> s {keywords = a} :: TestCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of characters that can exist between the end of at
-- least one complete character sequence specified by the keywords array
-- and the end of the text that matches the regex pattern. If a complete
-- keyword precedes all the text that matches the pattern and the keyword
-- is within the specified distance, Amazon Macie includes the result. The
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestCustomDataIdentifierResponse'
            Prelude.<$> (x Data..?> "matchCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestCustomDataIdentifier where
  hashWithSalt _salt TestCustomDataIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` ignoreWords
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` maximumMatchDistance
      `Prelude.hashWithSalt` regex
      `Prelude.hashWithSalt` sampleText

instance Prelude.NFData TestCustomDataIdentifier where
  rnf TestCustomDataIdentifier' {..} =
    Prelude.rnf ignoreWords `Prelude.seq`
      Prelude.rnf keywords `Prelude.seq`
        Prelude.rnf maximumMatchDistance `Prelude.seq`
          Prelude.rnf regex `Prelude.seq`
            Prelude.rnf sampleText

instance Data.ToHeaders TestCustomDataIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TestCustomDataIdentifier where
  toJSON TestCustomDataIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ignoreWords" Data..=) Prelude.<$> ignoreWords,
            ("keywords" Data..=) Prelude.<$> keywords,
            ("maximumMatchDistance" Data..=)
              Prelude.<$> maximumMatchDistance,
            Prelude.Just ("regex" Data..= regex),
            Prelude.Just ("sampleText" Data..= sampleText)
          ]
      )

instance Data.ToPath TestCustomDataIdentifier where
  toPath =
    Prelude.const "/custom-data-identifiers/test"

instance Data.ToQuery TestCustomDataIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestCustomDataIdentifierResponse' smart constructor.
data TestCustomDataIdentifierResponse = TestCustomDataIdentifierResponse'
  { -- | The number of occurrences of sample text that matched the criteria
    -- specified by the custom data identifier.
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
-- 'matchCount', 'testCustomDataIdentifierResponse_matchCount' - The number of occurrences of sample text that matched the criteria
-- specified by the custom data identifier.
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

-- | The number of occurrences of sample text that matched the criteria
-- specified by the custom data identifier.
testCustomDataIdentifierResponse_matchCount :: Lens.Lens' TestCustomDataIdentifierResponse (Prelude.Maybe Prelude.Int)
testCustomDataIdentifierResponse_matchCount = Lens.lens (\TestCustomDataIdentifierResponse' {matchCount} -> matchCount) (\s@TestCustomDataIdentifierResponse' {} a -> s {matchCount = a} :: TestCustomDataIdentifierResponse)

-- | The response's http status code.
testCustomDataIdentifierResponse_httpStatus :: Lens.Lens' TestCustomDataIdentifierResponse Prelude.Int
testCustomDataIdentifierResponse_httpStatus = Lens.lens (\TestCustomDataIdentifierResponse' {httpStatus} -> httpStatus) (\s@TestCustomDataIdentifierResponse' {} a -> s {httpStatus = a} :: TestCustomDataIdentifierResponse)

instance
  Prelude.NFData
    TestCustomDataIdentifierResponse
  where
  rnf TestCustomDataIdentifierResponse' {..} =
    Prelude.rnf matchCount `Prelude.seq`
      Prelude.rnf httpStatus
