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
-- Module      : Amazonka.Connect.CreateVocabulary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom vocabulary associated with your Amazon Connect
-- instance. You can set a custom vocabulary to be your default vocabulary
-- for a given language. Contact Lens for Amazon Connect uses the default
-- vocabulary in post-call and real-time contact analysis sessions for that
-- language.
module Amazonka.Connect.CreateVocabulary
  ( -- * Creating a Request
    CreateVocabulary (..),
    newCreateVocabulary,

    -- * Request Lenses
    createVocabulary_clientToken,
    createVocabulary_tags,
    createVocabulary_instanceId,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabulary_content,

    -- * Destructuring the Response
    CreateVocabularyResponse (..),
    newCreateVocabularyResponse,

    -- * Response Lenses
    createVocabularyResponse_httpStatus,
    createVocabularyResponse_vocabularyArn,
    createVocabularyResponse_vocabularyId,
    createVocabularyResponse_state,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVocabulary' smart constructor.
data CreateVocabulary = CreateVocabulary'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    -- If a create request is received more than once with same client token,
    -- subsequent requests return the previous response without creating a
    -- vocabulary again.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | A unique name of the custom vocabulary.
    vocabularyName :: Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
    languageCode :: VocabularyLanguageCode,
    -- | The content of the custom vocabulary in plain-text format with a table
    -- of values. Each row in the table represents a word or a phrase,
    -- described with @Phrase@, @IPA@, @SoundsLike@, and @DisplayAs@ fields.
    -- Separate the fields with TAB characters. The size limit is 50KB. For
    -- more information, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html#create-vocabulary-table Create a custom vocabulary using a table>.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVocabulary_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
-- If a create request is received more than once with same client token,
-- subsequent requests return the previous response without creating a
-- vocabulary again.
--
-- 'tags', 'createVocabulary_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'instanceId', 'createVocabulary_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'vocabularyName', 'createVocabulary_vocabularyName' - A unique name of the custom vocabulary.
--
-- 'languageCode', 'createVocabulary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
--
-- 'content', 'createVocabulary_content' - The content of the custom vocabulary in plain-text format with a table
-- of values. Each row in the table represents a word or a phrase,
-- described with @Phrase@, @IPA@, @SoundsLike@, and @DisplayAs@ fields.
-- Separate the fields with TAB characters. The size limit is 50KB. For
-- more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html#create-vocabulary-table Create a custom vocabulary using a table>.
newCreateVocabulary ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'vocabularyName'
  Prelude.Text ->
  -- | 'languageCode'
  VocabularyLanguageCode ->
  -- | 'content'
  Prelude.Text ->
  CreateVocabulary
newCreateVocabulary
  pInstanceId_
  pVocabularyName_
  pLanguageCode_
  pContent_ =
    CreateVocabulary'
      { clientToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        instanceId = pInstanceId_,
        vocabularyName = pVocabularyName_,
        languageCode = pLanguageCode_,
        content = pContent_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
-- If a create request is received more than once with same client token,
-- subsequent requests return the previous response without creating a
-- vocabulary again.
createVocabulary_clientToken :: Lens.Lens' CreateVocabulary (Prelude.Maybe Prelude.Text)
createVocabulary_clientToken = Lens.lens (\CreateVocabulary' {clientToken} -> clientToken) (\s@CreateVocabulary' {} a -> s {clientToken = a} :: CreateVocabulary)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
createVocabulary_tags :: Lens.Lens' CreateVocabulary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVocabulary_tags = Lens.lens (\CreateVocabulary' {tags} -> tags) (\s@CreateVocabulary' {} a -> s {tags = a} :: CreateVocabulary) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
createVocabulary_instanceId :: Lens.Lens' CreateVocabulary Prelude.Text
createVocabulary_instanceId = Lens.lens (\CreateVocabulary' {instanceId} -> instanceId) (\s@CreateVocabulary' {} a -> s {instanceId = a} :: CreateVocabulary)

-- | A unique name of the custom vocabulary.
createVocabulary_vocabularyName :: Lens.Lens' CreateVocabulary Prelude.Text
createVocabulary_vocabularyName = Lens.lens (\CreateVocabulary' {vocabularyName} -> vocabularyName) (\s@CreateVocabulary' {} a -> s {vocabularyName = a} :: CreateVocabulary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
createVocabulary_languageCode :: Lens.Lens' CreateVocabulary VocabularyLanguageCode
createVocabulary_languageCode = Lens.lens (\CreateVocabulary' {languageCode} -> languageCode) (\s@CreateVocabulary' {} a -> s {languageCode = a} :: CreateVocabulary)

-- | The content of the custom vocabulary in plain-text format with a table
-- of values. Each row in the table represents a word or a phrase,
-- described with @Phrase@, @IPA@, @SoundsLike@, and @DisplayAs@ fields.
-- Separate the fields with TAB characters. The size limit is 50KB. For
-- more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-vocabulary.html#create-vocabulary-table Create a custom vocabulary using a table>.
createVocabulary_content :: Lens.Lens' CreateVocabulary Prelude.Text
createVocabulary_content = Lens.lens (\CreateVocabulary' {content} -> content) (\s@CreateVocabulary' {} a -> s {content = a} :: CreateVocabulary)

instance Core.AWSRequest CreateVocabulary where
  type
    AWSResponse CreateVocabulary =
      CreateVocabularyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVocabularyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VocabularyArn")
            Prelude.<*> (x Data..:> "VocabularyId")
            Prelude.<*> (x Data..:> "State")
      )

instance Prelude.Hashable CreateVocabulary where
  hashWithSalt _salt CreateVocabulary' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` vocabularyName
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` content

instance Prelude.NFData CreateVocabulary where
  rnf CreateVocabulary' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf vocabularyName
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders CreateVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVocabulary where
  toJSON CreateVocabulary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("VocabularyName" Data..= vocabularyName),
            Prelude.Just ("LanguageCode" Data..= languageCode),
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath CreateVocabulary where
  toPath CreateVocabulary' {..} =
    Prelude.mconcat
      ["/vocabulary/", Data.toBS instanceId]

instance Data.ToQuery CreateVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVocabularyResponse' smart constructor.
data CreateVocabularyResponse = CreateVocabularyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the custom vocabulary.
    vocabularyArn :: Prelude.Text,
    -- | The identifier of the custom vocabulary.
    vocabularyId :: Prelude.Text,
    -- | The current state of the custom vocabulary.
    state :: VocabularyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createVocabularyResponse_httpStatus' - The response's http status code.
--
-- 'vocabularyArn', 'createVocabularyResponse_vocabularyArn' - The Amazon Resource Name (ARN) of the custom vocabulary.
--
-- 'vocabularyId', 'createVocabularyResponse_vocabularyId' - The identifier of the custom vocabulary.
--
-- 'state', 'createVocabularyResponse_state' - The current state of the custom vocabulary.
newCreateVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vocabularyArn'
  Prelude.Text ->
  -- | 'vocabularyId'
  Prelude.Text ->
  -- | 'state'
  VocabularyState ->
  CreateVocabularyResponse
newCreateVocabularyResponse
  pHttpStatus_
  pVocabularyArn_
  pVocabularyId_
  pState_ =
    CreateVocabularyResponse'
      { httpStatus =
          pHttpStatus_,
        vocabularyArn = pVocabularyArn_,
        vocabularyId = pVocabularyId_,
        state = pState_
      }

-- | The response's http status code.
createVocabularyResponse_httpStatus :: Lens.Lens' CreateVocabularyResponse Prelude.Int
createVocabularyResponse_httpStatus = Lens.lens (\CreateVocabularyResponse' {httpStatus} -> httpStatus) (\s@CreateVocabularyResponse' {} a -> s {httpStatus = a} :: CreateVocabularyResponse)

-- | The Amazon Resource Name (ARN) of the custom vocabulary.
createVocabularyResponse_vocabularyArn :: Lens.Lens' CreateVocabularyResponse Prelude.Text
createVocabularyResponse_vocabularyArn = Lens.lens (\CreateVocabularyResponse' {vocabularyArn} -> vocabularyArn) (\s@CreateVocabularyResponse' {} a -> s {vocabularyArn = a} :: CreateVocabularyResponse)

-- | The identifier of the custom vocabulary.
createVocabularyResponse_vocabularyId :: Lens.Lens' CreateVocabularyResponse Prelude.Text
createVocabularyResponse_vocabularyId = Lens.lens (\CreateVocabularyResponse' {vocabularyId} -> vocabularyId) (\s@CreateVocabularyResponse' {} a -> s {vocabularyId = a} :: CreateVocabularyResponse)

-- | The current state of the custom vocabulary.
createVocabularyResponse_state :: Lens.Lens' CreateVocabularyResponse VocabularyState
createVocabularyResponse_state = Lens.lens (\CreateVocabularyResponse' {state} -> state) (\s@CreateVocabularyResponse' {} a -> s {state = a} :: CreateVocabularyResponse)

instance Prelude.NFData CreateVocabularyResponse where
  rnf CreateVocabularyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vocabularyArn
      `Prelude.seq` Prelude.rnf vocabularyId
      `Prelude.seq` Prelude.rnf state
