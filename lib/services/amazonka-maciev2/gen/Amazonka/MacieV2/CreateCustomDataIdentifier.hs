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
-- Module      : Amazonka.MacieV2.CreateCustomDataIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and defines the criteria and other settings for a custom data
-- identifier.
module Amazonka.MacieV2.CreateCustomDataIdentifier
  ( -- * Creating a Request
    CreateCustomDataIdentifier (..),
    newCreateCustomDataIdentifier,

    -- * Request Lenses
    createCustomDataIdentifier_tags,
    createCustomDataIdentifier_clientToken,
    createCustomDataIdentifier_ignoreWords,
    createCustomDataIdentifier_keywords,
    createCustomDataIdentifier_description,
    createCustomDataIdentifier_severityLevels,
    createCustomDataIdentifier_maximumMatchDistance,
    createCustomDataIdentifier_name,
    createCustomDataIdentifier_regex,

    -- * Destructuring the Response
    CreateCustomDataIdentifierResponse (..),
    newCreateCustomDataIdentifierResponse,

    -- * Response Lenses
    createCustomDataIdentifierResponse_customDataIdentifierId,
    createCustomDataIdentifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomDataIdentifier' smart constructor.
data CreateCustomDataIdentifier = CreateCustomDataIdentifier'
  { -- | A map of key-value pairs that specifies the tags to associate with the
    -- custom data identifier.
    --
    -- A custom data identifier can have a maximum of 50 tags. Each tag
    -- consists of a tag key and an associated tag value. The maximum length of
    -- a tag key is 128 characters. The maximum length of a tag value is 256
    -- characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An array that lists specific character sequences (/ignore words/) to
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
    -- | A custom description of the custom data identifier. The description can
    -- contain as many as 512 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- description of a custom data identifier. Other users of your account
    -- might be able to see this description, depending on the actions that
    -- they\'re allowed to perform in Amazon Macie.
    description :: Prelude.Maybe Prelude.Text,
    -- | The severity to assign to findings that the custom data identifier
    -- produces, based on the number of occurrences of text that matches the
    -- custom data identifier\'s detection criteria. You can specify as many as
    -- three SeverityLevel objects in this array, one for each severity: LOW,
    -- MEDIUM, or HIGH. If you specify more than one, the occurrences
    -- thresholds must be in ascending order by severity, moving from LOW to
    -- HIGH. For example, 1 for LOW, 50 for MEDIUM, and 100 for HIGH. If an S3
    -- object contains fewer occurrences than the lowest specified threshold,
    -- Amazon Macie doesn\'t create a finding.
    --
    -- If you don\'t specify any values for this array, Macie creates findings
    -- for S3 objects that contain at least one occurrence of text that matches
    -- the detection criteria, and Macie assigns the MEDIUM severity to those
    -- findings.
    severityLevels :: Prelude.Maybe [SeverityLevel],
    -- | The maximum number of characters that can exist between the end of at
    -- least one complete character sequence specified by the keywords array
    -- and the end of the text that matches the regex pattern. If a complete
    -- keyword precedes all the text that matches the pattern and the keyword
    -- is within the specified distance, Amazon Macie includes the result. The
    -- distance can be 1-300 characters. The default value is 50.
    maximumMatchDistance :: Prelude.Maybe Prelude.Int,
    -- | A custom name for the custom data identifier. The name can contain as
    -- many as 128 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- name of a custom data identifier. Other users of your account might be
    -- able to see this name, depending on the actions that they\'re allowed to
    -- perform in Amazon Macie.
    name :: Prelude.Text,
    -- | The regular expression (/regex/) that defines the pattern to match. The
    -- expression can contain as many as 512 characters.
    regex :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomDataIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCustomDataIdentifier_tags' - A map of key-value pairs that specifies the tags to associate with the
-- custom data identifier.
--
-- A custom data identifier can have a maximum of 50 tags. Each tag
-- consists of a tag key and an associated tag value. The maximum length of
-- a tag key is 128 characters. The maximum length of a tag value is 256
-- characters.
--
-- 'clientToken', 'createCustomDataIdentifier_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'ignoreWords', 'createCustomDataIdentifier_ignoreWords' - An array that lists specific character sequences (/ignore words/) to
-- exclude from the results. If the text matched by the regular expression
-- contains any string in this array, Amazon Macie ignores it. The array
-- can contain as many as 10 ignore words. Each ignore word can contain
-- 4-90 UTF-8 characters. Ignore words are case sensitive.
--
-- 'keywords', 'createCustomDataIdentifier_keywords' - An array that lists specific character sequences (/keywords/), one of
-- which must precede and be within proximity (maximumMatchDistance) of the
-- regular expression to match. The array can contain as many as 50
-- keywords. Each keyword can contain 3-90 UTF-8 characters. Keywords
-- aren\'t case sensitive.
--
-- 'description', 'createCustomDataIdentifier_description' - A custom description of the custom data identifier. The description can
-- contain as many as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a custom data identifier. Other users of your account
-- might be able to see this description, depending on the actions that
-- they\'re allowed to perform in Amazon Macie.
--
-- 'severityLevels', 'createCustomDataIdentifier_severityLevels' - The severity to assign to findings that the custom data identifier
-- produces, based on the number of occurrences of text that matches the
-- custom data identifier\'s detection criteria. You can specify as many as
-- three SeverityLevel objects in this array, one for each severity: LOW,
-- MEDIUM, or HIGH. If you specify more than one, the occurrences
-- thresholds must be in ascending order by severity, moving from LOW to
-- HIGH. For example, 1 for LOW, 50 for MEDIUM, and 100 for HIGH. If an S3
-- object contains fewer occurrences than the lowest specified threshold,
-- Amazon Macie doesn\'t create a finding.
--
-- If you don\'t specify any values for this array, Macie creates findings
-- for S3 objects that contain at least one occurrence of text that matches
-- the detection criteria, and Macie assigns the MEDIUM severity to those
-- findings.
--
-- 'maximumMatchDistance', 'createCustomDataIdentifier_maximumMatchDistance' - The maximum number of characters that can exist between the end of at
-- least one complete character sequence specified by the keywords array
-- and the end of the text that matches the regex pattern. If a complete
-- keyword precedes all the text that matches the pattern and the keyword
-- is within the specified distance, Amazon Macie includes the result. The
-- distance can be 1-300 characters. The default value is 50.
--
-- 'name', 'createCustomDataIdentifier_name' - A custom name for the custom data identifier. The name can contain as
-- many as 128 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a custom data identifier. Other users of your account might be
-- able to see this name, depending on the actions that they\'re allowed to
-- perform in Amazon Macie.
--
-- 'regex', 'createCustomDataIdentifier_regex' - The regular expression (/regex/) that defines the pattern to match. The
-- expression can contain as many as 512 characters.
newCreateCustomDataIdentifier ::
  -- | 'name'
  Prelude.Text ->
  -- | 'regex'
  Prelude.Text ->
  CreateCustomDataIdentifier
newCreateCustomDataIdentifier pName_ pRegex_ =
  CreateCustomDataIdentifier'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      ignoreWords = Prelude.Nothing,
      keywords = Prelude.Nothing,
      description = Prelude.Nothing,
      severityLevels = Prelude.Nothing,
      maximumMatchDistance = Prelude.Nothing,
      name = pName_,
      regex = pRegex_
    }

-- | A map of key-value pairs that specifies the tags to associate with the
-- custom data identifier.
--
-- A custom data identifier can have a maximum of 50 tags. Each tag
-- consists of a tag key and an associated tag value. The maximum length of
-- a tag key is 128 characters. The maximum length of a tag value is 256
-- characters.
createCustomDataIdentifier_tags :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCustomDataIdentifier_tags = Lens.lens (\CreateCustomDataIdentifier' {tags} -> tags) (\s@CreateCustomDataIdentifier' {} a -> s {tags = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createCustomDataIdentifier_clientToken :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Text)
createCustomDataIdentifier_clientToken = Lens.lens (\CreateCustomDataIdentifier' {clientToken} -> clientToken) (\s@CreateCustomDataIdentifier' {} a -> s {clientToken = a} :: CreateCustomDataIdentifier)

-- | An array that lists specific character sequences (/ignore words/) to
-- exclude from the results. If the text matched by the regular expression
-- contains any string in this array, Amazon Macie ignores it. The array
-- can contain as many as 10 ignore words. Each ignore word can contain
-- 4-90 UTF-8 characters. Ignore words are case sensitive.
createCustomDataIdentifier_ignoreWords :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
createCustomDataIdentifier_ignoreWords = Lens.lens (\CreateCustomDataIdentifier' {ignoreWords} -> ignoreWords) (\s@CreateCustomDataIdentifier' {} a -> s {ignoreWords = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | An array that lists specific character sequences (/keywords/), one of
-- which must precede and be within proximity (maximumMatchDistance) of the
-- regular expression to match. The array can contain as many as 50
-- keywords. Each keyword can contain 3-90 UTF-8 characters. Keywords
-- aren\'t case sensitive.
createCustomDataIdentifier_keywords :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
createCustomDataIdentifier_keywords = Lens.lens (\CreateCustomDataIdentifier' {keywords} -> keywords) (\s@CreateCustomDataIdentifier' {} a -> s {keywords = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | A custom description of the custom data identifier. The description can
-- contain as many as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a custom data identifier. Other users of your account
-- might be able to see this description, depending on the actions that
-- they\'re allowed to perform in Amazon Macie.
createCustomDataIdentifier_description :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Text)
createCustomDataIdentifier_description = Lens.lens (\CreateCustomDataIdentifier' {description} -> description) (\s@CreateCustomDataIdentifier' {} a -> s {description = a} :: CreateCustomDataIdentifier)

-- | The severity to assign to findings that the custom data identifier
-- produces, based on the number of occurrences of text that matches the
-- custom data identifier\'s detection criteria. You can specify as many as
-- three SeverityLevel objects in this array, one for each severity: LOW,
-- MEDIUM, or HIGH. If you specify more than one, the occurrences
-- thresholds must be in ascending order by severity, moving from LOW to
-- HIGH. For example, 1 for LOW, 50 for MEDIUM, and 100 for HIGH. If an S3
-- object contains fewer occurrences than the lowest specified threshold,
-- Amazon Macie doesn\'t create a finding.
--
-- If you don\'t specify any values for this array, Macie creates findings
-- for S3 objects that contain at least one occurrence of text that matches
-- the detection criteria, and Macie assigns the MEDIUM severity to those
-- findings.
createCustomDataIdentifier_severityLevels :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe [SeverityLevel])
createCustomDataIdentifier_severityLevels = Lens.lens (\CreateCustomDataIdentifier' {severityLevels} -> severityLevels) (\s@CreateCustomDataIdentifier' {} a -> s {severityLevels = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of characters that can exist between the end of at
-- least one complete character sequence specified by the keywords array
-- and the end of the text that matches the regex pattern. If a complete
-- keyword precedes all the text that matches the pattern and the keyword
-- is within the specified distance, Amazon Macie includes the result. The
-- distance can be 1-300 characters. The default value is 50.
createCustomDataIdentifier_maximumMatchDistance :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Int)
createCustomDataIdentifier_maximumMatchDistance = Lens.lens (\CreateCustomDataIdentifier' {maximumMatchDistance} -> maximumMatchDistance) (\s@CreateCustomDataIdentifier' {} a -> s {maximumMatchDistance = a} :: CreateCustomDataIdentifier)

-- | A custom name for the custom data identifier. The name can contain as
-- many as 128 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a custom data identifier. Other users of your account might be
-- able to see this name, depending on the actions that they\'re allowed to
-- perform in Amazon Macie.
createCustomDataIdentifier_name :: Lens.Lens' CreateCustomDataIdentifier Prelude.Text
createCustomDataIdentifier_name = Lens.lens (\CreateCustomDataIdentifier' {name} -> name) (\s@CreateCustomDataIdentifier' {} a -> s {name = a} :: CreateCustomDataIdentifier)

-- | The regular expression (/regex/) that defines the pattern to match. The
-- expression can contain as many as 512 characters.
createCustomDataIdentifier_regex :: Lens.Lens' CreateCustomDataIdentifier Prelude.Text
createCustomDataIdentifier_regex = Lens.lens (\CreateCustomDataIdentifier' {regex} -> regex) (\s@CreateCustomDataIdentifier' {} a -> s {regex = a} :: CreateCustomDataIdentifier)

instance Core.AWSRequest CreateCustomDataIdentifier where
  type
    AWSResponse CreateCustomDataIdentifier =
      CreateCustomDataIdentifierResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomDataIdentifierResponse'
            Prelude.<$> (x Data..?> "customDataIdentifierId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomDataIdentifier where
  hashWithSalt _salt CreateCustomDataIdentifier' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` ignoreWords
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` severityLevels
      `Prelude.hashWithSalt` maximumMatchDistance
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regex

instance Prelude.NFData CreateCustomDataIdentifier where
  rnf CreateCustomDataIdentifier' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf ignoreWords
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf severityLevels
      `Prelude.seq` Prelude.rnf maximumMatchDistance
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regex

instance Data.ToHeaders CreateCustomDataIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomDataIdentifier where
  toJSON CreateCustomDataIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("ignoreWords" Data..=) Prelude.<$> ignoreWords,
            ("keywords" Data..=) Prelude.<$> keywords,
            ("description" Data..=) Prelude.<$> description,
            ("severityLevels" Data..=)
              Prelude.<$> severityLevels,
            ("maximumMatchDistance" Data..=)
              Prelude.<$> maximumMatchDistance,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("regex" Data..= regex)
          ]
      )

instance Data.ToPath CreateCustomDataIdentifier where
  toPath = Prelude.const "/custom-data-identifiers"

instance Data.ToQuery CreateCustomDataIdentifier where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomDataIdentifierResponse' smart constructor.
data CreateCustomDataIdentifierResponse = CreateCustomDataIdentifierResponse'
  { -- | The unique identifier for the custom data identifier that was created.
    customDataIdentifierId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomDataIdentifierResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDataIdentifierId', 'createCustomDataIdentifierResponse_customDataIdentifierId' - The unique identifier for the custom data identifier that was created.
--
-- 'httpStatus', 'createCustomDataIdentifierResponse_httpStatus' - The response's http status code.
newCreateCustomDataIdentifierResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomDataIdentifierResponse
newCreateCustomDataIdentifierResponse pHttpStatus_ =
  CreateCustomDataIdentifierResponse'
    { customDataIdentifierId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the custom data identifier that was created.
createCustomDataIdentifierResponse_customDataIdentifierId :: Lens.Lens' CreateCustomDataIdentifierResponse (Prelude.Maybe Prelude.Text)
createCustomDataIdentifierResponse_customDataIdentifierId = Lens.lens (\CreateCustomDataIdentifierResponse' {customDataIdentifierId} -> customDataIdentifierId) (\s@CreateCustomDataIdentifierResponse' {} a -> s {customDataIdentifierId = a} :: CreateCustomDataIdentifierResponse)

-- | The response's http status code.
createCustomDataIdentifierResponse_httpStatus :: Lens.Lens' CreateCustomDataIdentifierResponse Prelude.Int
createCustomDataIdentifierResponse_httpStatus = Lens.lens (\CreateCustomDataIdentifierResponse' {httpStatus} -> httpStatus) (\s@CreateCustomDataIdentifierResponse' {} a -> s {httpStatus = a} :: CreateCustomDataIdentifierResponse)

instance
  Prelude.NFData
    CreateCustomDataIdentifierResponse
  where
  rnf CreateCustomDataIdentifierResponse' {..} =
    Prelude.rnf customDataIdentifierId
      `Prelude.seq` Prelude.rnf httpStatus
