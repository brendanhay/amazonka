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
-- Module      : Amazonka.ComprehendMedical.InferICD10CM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- InferICD10CM detects medical conditions as entities listed in a patient
-- record and links those entities to normalized concept identifiers in the
-- ICD-10-CM knowledge base from the Centers for Disease Control. Amazon
-- Comprehend Medical only detects medical entities in English language
-- texts.
module Amazonka.ComprehendMedical.InferICD10CM
  ( -- * Creating a Request
    InferICD10CM (..),
    newInferICD10CM,

    -- * Request Lenses
    inferICD10CM_text,

    -- * Destructuring the Response
    InferICD10CMResponse (..),
    newInferICD10CMResponse,

    -- * Response Lenses
    inferICD10CMResponse_modelVersion,
    inferICD10CMResponse_paginationToken,
    inferICD10CMResponse_httpStatus,
    inferICD10CMResponse_entities,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInferICD10CM' smart constructor.
data InferICD10CM = InferICD10CM'
  { -- | The input text used for analysis. The input for InferICD10CM is a string
    -- from 1 to 10000 characters.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferICD10CM' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'inferICD10CM_text' - The input text used for analysis. The input for InferICD10CM is a string
-- from 1 to 10000 characters.
newInferICD10CM ::
  -- | 'text'
  Prelude.Text ->
  InferICD10CM
newInferICD10CM pText_ = InferICD10CM' {text = pText_}

-- | The input text used for analysis. The input for InferICD10CM is a string
-- from 1 to 10000 characters.
inferICD10CM_text :: Lens.Lens' InferICD10CM Prelude.Text
inferICD10CM_text = Lens.lens (\InferICD10CM' {text} -> text) (\s@InferICD10CM' {} a -> s {text = a} :: InferICD10CM)

instance Core.AWSRequest InferICD10CM where
  type AWSResponse InferICD10CM = InferICD10CMResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InferICD10CMResponse'
            Prelude.<$> (x Data..?> "ModelVersion")
            Prelude.<*> (x Data..?> "PaginationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable InferICD10CM where
  hashWithSalt _salt InferICD10CM' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData InferICD10CM where
  rnf InferICD10CM' {..} = Prelude.rnf text

instance Data.ToHeaders InferICD10CM where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.InferICD10CM" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InferICD10CM where
  toJSON InferICD10CM' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Text" Data..= text)]
      )

instance Data.ToPath InferICD10CM where
  toPath = Prelude.const "/"

instance Data.ToQuery InferICD10CM where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInferICD10CMResponse' smart constructor.
data InferICD10CMResponse = InferICD10CMResponse'
  { -- | The version of the model used to analyze the documents, in the format
    -- /n/./n/./n/ You can use this information to track the model used for a
    -- particular batch of documents.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous request to @InferICD10CM@ was truncated,
    -- include the @PaginationToken@ to fetch the next page of medical
    -- condition entities.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The medical conditions detected in the text linked to ICD-10-CM
    -- concepts. If the action is successful, the service sends back an HTTP
    -- 200 response, as well as the entities detected.
    entities :: [ICD10CMEntity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferICD10CMResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersion', 'inferICD10CMResponse_modelVersion' - The version of the model used to analyze the documents, in the format
-- /n/./n/./n/ You can use this information to track the model used for a
-- particular batch of documents.
--
-- 'paginationToken', 'inferICD10CMResponse_paginationToken' - If the result of the previous request to @InferICD10CM@ was truncated,
-- include the @PaginationToken@ to fetch the next page of medical
-- condition entities.
--
-- 'httpStatus', 'inferICD10CMResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'inferICD10CMResponse_entities' - The medical conditions detected in the text linked to ICD-10-CM
-- concepts. If the action is successful, the service sends back an HTTP
-- 200 response, as well as the entities detected.
newInferICD10CMResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InferICD10CMResponse
newInferICD10CMResponse pHttpStatus_ =
  InferICD10CMResponse'
    { modelVersion =
        Prelude.Nothing,
      paginationToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The version of the model used to analyze the documents, in the format
-- /n/./n/./n/ You can use this information to track the model used for a
-- particular batch of documents.
inferICD10CMResponse_modelVersion :: Lens.Lens' InferICD10CMResponse (Prelude.Maybe Prelude.Text)
inferICD10CMResponse_modelVersion = Lens.lens (\InferICD10CMResponse' {modelVersion} -> modelVersion) (\s@InferICD10CMResponse' {} a -> s {modelVersion = a} :: InferICD10CMResponse)

-- | If the result of the previous request to @InferICD10CM@ was truncated,
-- include the @PaginationToken@ to fetch the next page of medical
-- condition entities.
inferICD10CMResponse_paginationToken :: Lens.Lens' InferICD10CMResponse (Prelude.Maybe Prelude.Text)
inferICD10CMResponse_paginationToken = Lens.lens (\InferICD10CMResponse' {paginationToken} -> paginationToken) (\s@InferICD10CMResponse' {} a -> s {paginationToken = a} :: InferICD10CMResponse)

-- | The response's http status code.
inferICD10CMResponse_httpStatus :: Lens.Lens' InferICD10CMResponse Prelude.Int
inferICD10CMResponse_httpStatus = Lens.lens (\InferICD10CMResponse' {httpStatus} -> httpStatus) (\s@InferICD10CMResponse' {} a -> s {httpStatus = a} :: InferICD10CMResponse)

-- | The medical conditions detected in the text linked to ICD-10-CM
-- concepts. If the action is successful, the service sends back an HTTP
-- 200 response, as well as the entities detected.
inferICD10CMResponse_entities :: Lens.Lens' InferICD10CMResponse [ICD10CMEntity]
inferICD10CMResponse_entities = Lens.lens (\InferICD10CMResponse' {entities} -> entities) (\s@InferICD10CMResponse' {} a -> s {entities = a} :: InferICD10CMResponse) Prelude.. Lens.coerced

instance Prelude.NFData InferICD10CMResponse where
  rnf InferICD10CMResponse' {..} =
    Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
