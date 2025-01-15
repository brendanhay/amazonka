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
-- Module      : Amazonka.ComprehendMedical.InferRxNorm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- InferRxNorm detects medications as entities listed in a patient record
-- and links to the normalized concept identifiers in the RxNorm database
-- from the National Library of Medicine. Amazon Comprehend Medical only
-- detects medical entities in English language texts.
module Amazonka.ComprehendMedical.InferRxNorm
  ( -- * Creating a Request
    InferRxNorm (..),
    newInferRxNorm,

    -- * Request Lenses
    inferRxNorm_text,

    -- * Destructuring the Response
    InferRxNormResponse (..),
    newInferRxNormResponse,

    -- * Response Lenses
    inferRxNormResponse_modelVersion,
    inferRxNormResponse_paginationToken,
    inferRxNormResponse_httpStatus,
    inferRxNormResponse_entities,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInferRxNorm' smart constructor.
data InferRxNorm = InferRxNorm'
  { -- | The input text used for analysis. The input for InferRxNorm is a string
    -- from 1 to 10000 characters.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferRxNorm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'inferRxNorm_text' - The input text used for analysis. The input for InferRxNorm is a string
-- from 1 to 10000 characters.
newInferRxNorm ::
  -- | 'text'
  Prelude.Text ->
  InferRxNorm
newInferRxNorm pText_ = InferRxNorm' {text = pText_}

-- | The input text used for analysis. The input for InferRxNorm is a string
-- from 1 to 10000 characters.
inferRxNorm_text :: Lens.Lens' InferRxNorm Prelude.Text
inferRxNorm_text = Lens.lens (\InferRxNorm' {text} -> text) (\s@InferRxNorm' {} a -> s {text = a} :: InferRxNorm)

instance Core.AWSRequest InferRxNorm where
  type AWSResponse InferRxNorm = InferRxNormResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InferRxNormResponse'
            Prelude.<$> (x Data..?> "ModelVersion")
            Prelude.<*> (x Data..?> "PaginationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable InferRxNorm where
  hashWithSalt _salt InferRxNorm' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData InferRxNorm where
  rnf InferRxNorm' {..} = Prelude.rnf text

instance Data.ToHeaders InferRxNorm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.InferRxNorm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InferRxNorm where
  toJSON InferRxNorm' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Text" Data..= text)]
      )

instance Data.ToPath InferRxNorm where
  toPath = Prelude.const "/"

instance Data.ToQuery InferRxNorm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInferRxNormResponse' smart constructor.
data InferRxNormResponse = InferRxNormResponse'
  { -- | The version of the model used to analyze the documents, in the format
    -- /n/./n/./n/ You can use this information to track the model used for a
    -- particular batch of documents.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous request to @InferRxNorm@ was truncated,
    -- include the @PaginationToken@ to fetch the next page of medication
    -- entities.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The medication entities detected in the text linked to RxNorm concepts.
    -- If the action is successful, the service sends back an HTTP 200
    -- response, as well as the entities detected.
    entities :: [RxNormEntity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferRxNormResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersion', 'inferRxNormResponse_modelVersion' - The version of the model used to analyze the documents, in the format
-- /n/./n/./n/ You can use this information to track the model used for a
-- particular batch of documents.
--
-- 'paginationToken', 'inferRxNormResponse_paginationToken' - If the result of the previous request to @InferRxNorm@ was truncated,
-- include the @PaginationToken@ to fetch the next page of medication
-- entities.
--
-- 'httpStatus', 'inferRxNormResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'inferRxNormResponse_entities' - The medication entities detected in the text linked to RxNorm concepts.
-- If the action is successful, the service sends back an HTTP 200
-- response, as well as the entities detected.
newInferRxNormResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InferRxNormResponse
newInferRxNormResponse pHttpStatus_ =
  InferRxNormResponse'
    { modelVersion =
        Prelude.Nothing,
      paginationToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The version of the model used to analyze the documents, in the format
-- /n/./n/./n/ You can use this information to track the model used for a
-- particular batch of documents.
inferRxNormResponse_modelVersion :: Lens.Lens' InferRxNormResponse (Prelude.Maybe Prelude.Text)
inferRxNormResponse_modelVersion = Lens.lens (\InferRxNormResponse' {modelVersion} -> modelVersion) (\s@InferRxNormResponse' {} a -> s {modelVersion = a} :: InferRxNormResponse)

-- | If the result of the previous request to @InferRxNorm@ was truncated,
-- include the @PaginationToken@ to fetch the next page of medication
-- entities.
inferRxNormResponse_paginationToken :: Lens.Lens' InferRxNormResponse (Prelude.Maybe Prelude.Text)
inferRxNormResponse_paginationToken = Lens.lens (\InferRxNormResponse' {paginationToken} -> paginationToken) (\s@InferRxNormResponse' {} a -> s {paginationToken = a} :: InferRxNormResponse)

-- | The response's http status code.
inferRxNormResponse_httpStatus :: Lens.Lens' InferRxNormResponse Prelude.Int
inferRxNormResponse_httpStatus = Lens.lens (\InferRxNormResponse' {httpStatus} -> httpStatus) (\s@InferRxNormResponse' {} a -> s {httpStatus = a} :: InferRxNormResponse)

-- | The medication entities detected in the text linked to RxNorm concepts.
-- If the action is successful, the service sends back an HTTP 200
-- response, as well as the entities detected.
inferRxNormResponse_entities :: Lens.Lens' InferRxNormResponse [RxNormEntity]
inferRxNormResponse_entities = Lens.lens (\InferRxNormResponse' {entities} -> entities) (\s@InferRxNormResponse' {} a -> s {entities = a} :: InferRxNormResponse) Prelude.. Lens.coerced

instance Prelude.NFData InferRxNormResponse where
  rnf InferRxNormResponse' {..} =
    Prelude.rnf modelVersion `Prelude.seq`
      Prelude.rnf paginationToken `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf entities
