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
-- Module      : Amazonka.Connect.AssociateDefaultVocabulary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an existing vocabulary as the default. Contact Lens for
-- Amazon Connect uses the vocabulary in post-call and real-time analysis
-- sessions for the given language.
module Amazonka.Connect.AssociateDefaultVocabulary
  ( -- * Creating a Request
    AssociateDefaultVocabulary (..),
    newAssociateDefaultVocabulary,

    -- * Request Lenses
    associateDefaultVocabulary_vocabularyId,
    associateDefaultVocabulary_instanceId,
    associateDefaultVocabulary_languageCode,

    -- * Destructuring the Response
    AssociateDefaultVocabularyResponse (..),
    newAssociateDefaultVocabularyResponse,

    -- * Response Lenses
    associateDefaultVocabularyResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateDefaultVocabulary' smart constructor.
data AssociateDefaultVocabulary = AssociateDefaultVocabulary'
  { -- | The identifier of the custom vocabulary. If this is empty, the default
    -- is set to none.
    vocabularyId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
    languageCode :: VocabularyLanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDefaultVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vocabularyId', 'associateDefaultVocabulary_vocabularyId' - The identifier of the custom vocabulary. If this is empty, the default
-- is set to none.
--
-- 'instanceId', 'associateDefaultVocabulary_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'languageCode', 'associateDefaultVocabulary_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
newAssociateDefaultVocabulary ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'languageCode'
  VocabularyLanguageCode ->
  AssociateDefaultVocabulary
newAssociateDefaultVocabulary
  pInstanceId_
  pLanguageCode_ =
    AssociateDefaultVocabulary'
      { vocabularyId =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        languageCode = pLanguageCode_
      }

-- | The identifier of the custom vocabulary. If this is empty, the default
-- is set to none.
associateDefaultVocabulary_vocabularyId :: Lens.Lens' AssociateDefaultVocabulary (Prelude.Maybe Prelude.Text)
associateDefaultVocabulary_vocabularyId = Lens.lens (\AssociateDefaultVocabulary' {vocabularyId} -> vocabularyId) (\s@AssociateDefaultVocabulary' {} a -> s {vocabularyId = a} :: AssociateDefaultVocabulary)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
associateDefaultVocabulary_instanceId :: Lens.Lens' AssociateDefaultVocabulary Prelude.Text
associateDefaultVocabulary_instanceId = Lens.lens (\AssociateDefaultVocabulary' {instanceId} -> instanceId) (\s@AssociateDefaultVocabulary' {} a -> s {instanceId = a} :: AssociateDefaultVocabulary)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
associateDefaultVocabulary_languageCode :: Lens.Lens' AssociateDefaultVocabulary VocabularyLanguageCode
associateDefaultVocabulary_languageCode = Lens.lens (\AssociateDefaultVocabulary' {languageCode} -> languageCode) (\s@AssociateDefaultVocabulary' {} a -> s {languageCode = a} :: AssociateDefaultVocabulary)

instance Core.AWSRequest AssociateDefaultVocabulary where
  type
    AWSResponse AssociateDefaultVocabulary =
      AssociateDefaultVocabularyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDefaultVocabularyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDefaultVocabulary where
  hashWithSalt _salt AssociateDefaultVocabulary' {..} =
    _salt `Prelude.hashWithSalt` vocabularyId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData AssociateDefaultVocabulary where
  rnf AssociateDefaultVocabulary' {..} =
    Prelude.rnf vocabularyId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders AssociateDefaultVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateDefaultVocabulary where
  toJSON AssociateDefaultVocabulary' {..} =
    Core.object
      ( Prelude.catMaybes
          [("VocabularyId" Core..=) Prelude.<$> vocabularyId]
      )

instance Core.ToPath AssociateDefaultVocabulary where
  toPath AssociateDefaultVocabulary' {..} =
    Prelude.mconcat
      [ "/default-vocabulary/",
        Core.toBS instanceId,
        "/",
        Core.toBS languageCode
      ]

instance Core.ToQuery AssociateDefaultVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDefaultVocabularyResponse' smart constructor.
data AssociateDefaultVocabularyResponse = AssociateDefaultVocabularyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDefaultVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDefaultVocabularyResponse_httpStatus' - The response's http status code.
newAssociateDefaultVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDefaultVocabularyResponse
newAssociateDefaultVocabularyResponse pHttpStatus_ =
  AssociateDefaultVocabularyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDefaultVocabularyResponse_httpStatus :: Lens.Lens' AssociateDefaultVocabularyResponse Prelude.Int
associateDefaultVocabularyResponse_httpStatus = Lens.lens (\AssociateDefaultVocabularyResponse' {httpStatus} -> httpStatus) (\s@AssociateDefaultVocabularyResponse' {} a -> s {httpStatus = a} :: AssociateDefaultVocabularyResponse)

instance
  Prelude.NFData
    AssociateDefaultVocabularyResponse
  where
  rnf AssociateDefaultVocabularyResponse' {..} =
    Prelude.rnf httpStatus
