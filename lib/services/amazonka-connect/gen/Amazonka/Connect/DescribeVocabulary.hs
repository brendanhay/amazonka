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
-- Module      : Amazonka.Connect.DescribeVocabulary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified vocabulary.
module Amazonka.Connect.DescribeVocabulary
  ( -- * Creating a Request
    DescribeVocabulary (..),
    newDescribeVocabulary,

    -- * Request Lenses
    describeVocabulary_instanceId,
    describeVocabulary_vocabularyId,

    -- * Destructuring the Response
    DescribeVocabularyResponse (..),
    newDescribeVocabularyResponse,

    -- * Response Lenses
    describeVocabularyResponse_httpStatus,
    describeVocabularyResponse_vocabulary,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVocabulary' smart constructor.
data DescribeVocabulary = DescribeVocabulary'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the custom vocabulary.
    vocabularyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVocabulary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeVocabulary_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'vocabularyId', 'describeVocabulary_vocabularyId' - The identifier of the custom vocabulary.
newDescribeVocabulary ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'vocabularyId'
  Prelude.Text ->
  DescribeVocabulary
newDescribeVocabulary pInstanceId_ pVocabularyId_ =
  DescribeVocabulary'
    { instanceId = pInstanceId_,
      vocabularyId = pVocabularyId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeVocabulary_instanceId :: Lens.Lens' DescribeVocabulary Prelude.Text
describeVocabulary_instanceId = Lens.lens (\DescribeVocabulary' {instanceId} -> instanceId) (\s@DescribeVocabulary' {} a -> s {instanceId = a} :: DescribeVocabulary)

-- | The identifier of the custom vocabulary.
describeVocabulary_vocabularyId :: Lens.Lens' DescribeVocabulary Prelude.Text
describeVocabulary_vocabularyId = Lens.lens (\DescribeVocabulary' {vocabularyId} -> vocabularyId) (\s@DescribeVocabulary' {} a -> s {vocabularyId = a} :: DescribeVocabulary)

instance Core.AWSRequest DescribeVocabulary where
  type
    AWSResponse DescribeVocabulary =
      DescribeVocabularyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVocabularyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Vocabulary")
      )

instance Prelude.Hashable DescribeVocabulary where
  hashWithSalt _salt DescribeVocabulary' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` vocabularyId

instance Prelude.NFData DescribeVocabulary where
  rnf DescribeVocabulary' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf vocabularyId

instance Data.ToHeaders DescribeVocabulary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVocabulary where
  toPath DescribeVocabulary' {..} =
    Prelude.mconcat
      [ "/vocabulary/",
        Data.toBS instanceId,
        "/",
        Data.toBS vocabularyId
      ]

instance Data.ToQuery DescribeVocabulary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVocabularyResponse' smart constructor.
data DescribeVocabularyResponse = DescribeVocabularyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of specific words that you want Contact Lens for Amazon Connect
    -- to recognize in your audio input. They are generally domain-specific
    -- words and phrases, words that Contact Lens is not recognizing, or proper
    -- nouns.
    vocabulary :: Vocabulary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVocabularyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVocabularyResponse_httpStatus' - The response's http status code.
--
-- 'vocabulary', 'describeVocabularyResponse_vocabulary' - A list of specific words that you want Contact Lens for Amazon Connect
-- to recognize in your audio input. They are generally domain-specific
-- words and phrases, words that Contact Lens is not recognizing, or proper
-- nouns.
newDescribeVocabularyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vocabulary'
  Vocabulary ->
  DescribeVocabularyResponse
newDescribeVocabularyResponse
  pHttpStatus_
  pVocabulary_ =
    DescribeVocabularyResponse'
      { httpStatus =
          pHttpStatus_,
        vocabulary = pVocabulary_
      }

-- | The response's http status code.
describeVocabularyResponse_httpStatus :: Lens.Lens' DescribeVocabularyResponse Prelude.Int
describeVocabularyResponse_httpStatus = Lens.lens (\DescribeVocabularyResponse' {httpStatus} -> httpStatus) (\s@DescribeVocabularyResponse' {} a -> s {httpStatus = a} :: DescribeVocabularyResponse)

-- | A list of specific words that you want Contact Lens for Amazon Connect
-- to recognize in your audio input. They are generally domain-specific
-- words and phrases, words that Contact Lens is not recognizing, or proper
-- nouns.
describeVocabularyResponse_vocabulary :: Lens.Lens' DescribeVocabularyResponse Vocabulary
describeVocabularyResponse_vocabulary = Lens.lens (\DescribeVocabularyResponse' {vocabulary} -> vocabulary) (\s@DescribeVocabularyResponse' {} a -> s {vocabulary = a} :: DescribeVocabularyResponse)

instance Prelude.NFData DescribeVocabularyResponse where
  rnf DescribeVocabularyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vocabulary
