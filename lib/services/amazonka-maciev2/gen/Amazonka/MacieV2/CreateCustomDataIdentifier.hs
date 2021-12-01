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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    createCustomDataIdentifier_clientToken,
    createCustomDataIdentifier_regex,
    createCustomDataIdentifier_name,
    createCustomDataIdentifier_keywords,
    createCustomDataIdentifier_ignoreWords,
    createCustomDataIdentifier_maximumMatchDistance,
    createCustomDataIdentifier_description,
    createCustomDataIdentifier_tags,

    -- * Destructuring the Response
    CreateCustomDataIdentifierResponse (..),
    newCreateCustomDataIdentifierResponse,

    -- * Response Lenses
    createCustomDataIdentifierResponse_customDataIdentifierId,
    createCustomDataIdentifierResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomDataIdentifier' smart constructor.
data CreateCustomDataIdentifier = CreateCustomDataIdentifier'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The regular expression (/regex/) that defines the pattern to match. The
    -- expression can contain as many as 512 characters.
    regex :: Prelude.Maybe Prelude.Text,
    -- | A custom name for the custom data identifier. The name can contain as
    -- many as 128 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- name of a custom data identifier. Other users of your account might be
    -- able to see the identifier\'s name, depending on the actions that
    -- they\'re allowed to perform in Amazon Macie.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array that lists specific character sequences (keywords), one of
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
    -- | A custom description of the custom data identifier. The description can
    -- contain as many as 512 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- description of a custom data identifier. Other users of your account
    -- might be able to see the identifier\'s description, depending on the
    -- actions that they\'re allowed to perform in Amazon Macie.
    description :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs that specifies the tags to associate with the
    -- custom data identifier.
    --
    -- A custom data identifier can have a maximum of 50 tags. Each tag
    -- consists of a tag key and an associated tag value. The maximum length of
    -- a tag key is 128 characters. The maximum length of a tag value is 256
    -- characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'clientToken', 'createCustomDataIdentifier_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'regex', 'createCustomDataIdentifier_regex' - The regular expression (/regex/) that defines the pattern to match. The
-- expression can contain as many as 512 characters.
--
-- 'name', 'createCustomDataIdentifier_name' - A custom name for the custom data identifier. The name can contain as
-- many as 128 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a custom data identifier. Other users of your account might be
-- able to see the identifier\'s name, depending on the actions that
-- they\'re allowed to perform in Amazon Macie.
--
-- 'keywords', 'createCustomDataIdentifier_keywords' - An array that lists specific character sequences (keywords), one of
-- which must be within proximity (maximumMatchDistance) of the regular
-- expression to match. The array can contain as many as 50 keywords. Each
-- keyword can contain 3-90 UTF-8 characters. Keywords aren\'t case
-- sensitive.
--
-- 'ignoreWords', 'createCustomDataIdentifier_ignoreWords' - An array that lists specific character sequences (ignore words) to
-- exclude from the results. If the text matched by the regular expression
-- is the same as any string in this array, Amazon Macie ignores it. The
-- array can contain as many as 10 ignore words. Each ignore word can
-- contain 4-90 UTF-8 characters. Ignore words are case sensitive.
--
-- 'maximumMatchDistance', 'createCustomDataIdentifier_maximumMatchDistance' - The maximum number of characters that can exist between text that
-- matches the regex pattern and the character sequences specified by the
-- keywords array. Amazon Macie includes or excludes a result based on the
-- proximity of a keyword to text that matches the regex pattern. The
-- distance can be 1-300 characters. The default value is 50.
--
-- 'description', 'createCustomDataIdentifier_description' - A custom description of the custom data identifier. The description can
-- contain as many as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a custom data identifier. Other users of your account
-- might be able to see the identifier\'s description, depending on the
-- actions that they\'re allowed to perform in Amazon Macie.
--
-- 'tags', 'createCustomDataIdentifier_tags' - A map of key-value pairs that specifies the tags to associate with the
-- custom data identifier.
--
-- A custom data identifier can have a maximum of 50 tags. Each tag
-- consists of a tag key and an associated tag value. The maximum length of
-- a tag key is 128 characters. The maximum length of a tag value is 256
-- characters.
newCreateCustomDataIdentifier ::
  CreateCustomDataIdentifier
newCreateCustomDataIdentifier =
  CreateCustomDataIdentifier'
    { clientToken =
        Prelude.Nothing,
      regex = Prelude.Nothing,
      name = Prelude.Nothing,
      keywords = Prelude.Nothing,
      ignoreWords = Prelude.Nothing,
      maximumMatchDistance = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createCustomDataIdentifier_clientToken :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Text)
createCustomDataIdentifier_clientToken = Lens.lens (\CreateCustomDataIdentifier' {clientToken} -> clientToken) (\s@CreateCustomDataIdentifier' {} a -> s {clientToken = a} :: CreateCustomDataIdentifier)

-- | The regular expression (/regex/) that defines the pattern to match. The
-- expression can contain as many as 512 characters.
createCustomDataIdentifier_regex :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Text)
createCustomDataIdentifier_regex = Lens.lens (\CreateCustomDataIdentifier' {regex} -> regex) (\s@CreateCustomDataIdentifier' {} a -> s {regex = a} :: CreateCustomDataIdentifier)

-- | A custom name for the custom data identifier. The name can contain as
-- many as 128 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a custom data identifier. Other users of your account might be
-- able to see the identifier\'s name, depending on the actions that
-- they\'re allowed to perform in Amazon Macie.
createCustomDataIdentifier_name :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Text)
createCustomDataIdentifier_name = Lens.lens (\CreateCustomDataIdentifier' {name} -> name) (\s@CreateCustomDataIdentifier' {} a -> s {name = a} :: CreateCustomDataIdentifier)

-- | An array that lists specific character sequences (keywords), one of
-- which must be within proximity (maximumMatchDistance) of the regular
-- expression to match. The array can contain as many as 50 keywords. Each
-- keyword can contain 3-90 UTF-8 characters. Keywords aren\'t case
-- sensitive.
createCustomDataIdentifier_keywords :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
createCustomDataIdentifier_keywords = Lens.lens (\CreateCustomDataIdentifier' {keywords} -> keywords) (\s@CreateCustomDataIdentifier' {} a -> s {keywords = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | An array that lists specific character sequences (ignore words) to
-- exclude from the results. If the text matched by the regular expression
-- is the same as any string in this array, Amazon Macie ignores it. The
-- array can contain as many as 10 ignore words. Each ignore word can
-- contain 4-90 UTF-8 characters. Ignore words are case sensitive.
createCustomDataIdentifier_ignoreWords :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe [Prelude.Text])
createCustomDataIdentifier_ignoreWords = Lens.lens (\CreateCustomDataIdentifier' {ignoreWords} -> ignoreWords) (\s@CreateCustomDataIdentifier' {} a -> s {ignoreWords = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of characters that can exist between text that
-- matches the regex pattern and the character sequences specified by the
-- keywords array. Amazon Macie includes or excludes a result based on the
-- proximity of a keyword to text that matches the regex pattern. The
-- distance can be 1-300 characters. The default value is 50.
createCustomDataIdentifier_maximumMatchDistance :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Int)
createCustomDataIdentifier_maximumMatchDistance = Lens.lens (\CreateCustomDataIdentifier' {maximumMatchDistance} -> maximumMatchDistance) (\s@CreateCustomDataIdentifier' {} a -> s {maximumMatchDistance = a} :: CreateCustomDataIdentifier)

-- | A custom description of the custom data identifier. The description can
-- contain as many as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a custom data identifier. Other users of your account
-- might be able to see the identifier\'s description, depending on the
-- actions that they\'re allowed to perform in Amazon Macie.
createCustomDataIdentifier_description :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe Prelude.Text)
createCustomDataIdentifier_description = Lens.lens (\CreateCustomDataIdentifier' {description} -> description) (\s@CreateCustomDataIdentifier' {} a -> s {description = a} :: CreateCustomDataIdentifier)

-- | A map of key-value pairs that specifies the tags to associate with the
-- custom data identifier.
--
-- A custom data identifier can have a maximum of 50 tags. Each tag
-- consists of a tag key and an associated tag value. The maximum length of
-- a tag key is 128 characters. The maximum length of a tag value is 256
-- characters.
createCustomDataIdentifier_tags :: Lens.Lens' CreateCustomDataIdentifier (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCustomDataIdentifier_tags = Lens.lens (\CreateCustomDataIdentifier' {tags} -> tags) (\s@CreateCustomDataIdentifier' {} a -> s {tags = a} :: CreateCustomDataIdentifier) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateCustomDataIdentifier where
  type
    AWSResponse CreateCustomDataIdentifier =
      CreateCustomDataIdentifierResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomDataIdentifierResponse'
            Prelude.<$> (x Core..?> "customDataIdentifierId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomDataIdentifier where
  hashWithSalt salt' CreateCustomDataIdentifier' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maximumMatchDistance
      `Prelude.hashWithSalt` ignoreWords
      `Prelude.hashWithSalt` keywords
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regex
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateCustomDataIdentifier where
  rnf CreateCustomDataIdentifier' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maximumMatchDistance
      `Prelude.seq` Prelude.rnf ignoreWords
      `Prelude.seq` Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regex

instance Core.ToHeaders CreateCustomDataIdentifier where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCustomDataIdentifier where
  toJSON CreateCustomDataIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("regex" Core..=) Prelude.<$> regex,
            ("name" Core..=) Prelude.<$> name,
            ("keywords" Core..=) Prelude.<$> keywords,
            ("ignoreWords" Core..=) Prelude.<$> ignoreWords,
            ("maximumMatchDistance" Core..=)
              Prelude.<$> maximumMatchDistance,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateCustomDataIdentifier where
  toPath = Prelude.const "/custom-data-identifiers"

instance Core.ToQuery CreateCustomDataIdentifier where
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
