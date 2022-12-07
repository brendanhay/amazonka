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
-- Module      : Amazonka.ComprehendMedical.DetectPHI
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the clinical text for protected health information (PHI)
-- entities and returns the entity category, location, and confidence score
-- for each entity. Amazon Comprehend Medical only detects entities in
-- English language texts.
module Amazonka.ComprehendMedical.DetectPHI
  ( -- * Creating a Request
    DetectPHI (..),
    newDetectPHI,

    -- * Request Lenses
    detectPHI_text,

    -- * Destructuring the Response
    DetectPHIResponse (..),
    newDetectPHIResponse,

    -- * Response Lenses
    detectPHIResponse_paginationToken,
    detectPHIResponse_httpStatus,
    detectPHIResponse_entities,
    detectPHIResponse_modelVersion,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectPHI' smart constructor.
data DetectPHI = DetectPHI'
  { -- | A UTF-8 text string containing the clinical content being examined for
    -- PHI entities. Each string must contain fewer than 20,000 bytes of
    -- characters.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectPHI' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectPHI_text' - A UTF-8 text string containing the clinical content being examined for
-- PHI entities. Each string must contain fewer than 20,000 bytes of
-- characters.
newDetectPHI ::
  -- | 'text'
  Prelude.Text ->
  DetectPHI
newDetectPHI pText_ = DetectPHI' {text = pText_}

-- | A UTF-8 text string containing the clinical content being examined for
-- PHI entities. Each string must contain fewer than 20,000 bytes of
-- characters.
detectPHI_text :: Lens.Lens' DetectPHI Prelude.Text
detectPHI_text = Lens.lens (\DetectPHI' {text} -> text) (\s@DetectPHI' {} a -> s {text = a} :: DetectPHI)

instance Core.AWSRequest DetectPHI where
  type AWSResponse DetectPHI = DetectPHIResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectPHIResponse'
            Prelude.<$> (x Data..?> "PaginationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "ModelVersion")
      )

instance Prelude.Hashable DetectPHI where
  hashWithSalt _salt DetectPHI' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData DetectPHI where
  rnf DetectPHI' {..} = Prelude.rnf text

instance Data.ToHeaders DetectPHI where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.DetectPHI" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectPHI where
  toJSON DetectPHI' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Text" Data..= text)]
      )

instance Data.ToPath DetectPHI where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectPHI where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectPHIResponse' smart constructor.
data DetectPHIResponse = DetectPHIResponse'
  { -- | If the result of the previous request to @DetectPHI@ was truncated,
    -- include the @PaginationToken@ to fetch the next page of PHI entities.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The collection of PHI entities extracted from the input text and their
    -- associated information. For each entity, the response provides the
    -- entity text, the entity category, where the entity text begins and ends,
    -- and the level of confidence that Comprehend Medical; has in its
    -- detection.
    entities :: [Entity],
    -- | The version of the model used to analyze the documents. The version
    -- number looks like X.X.X. You can use this information to track the model
    -- used for a particular batch of documents.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectPHIResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'detectPHIResponse_paginationToken' - If the result of the previous request to @DetectPHI@ was truncated,
-- include the @PaginationToken@ to fetch the next page of PHI entities.
--
-- 'httpStatus', 'detectPHIResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'detectPHIResponse_entities' - The collection of PHI entities extracted from the input text and their
-- associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Comprehend Medical; has in its
-- detection.
--
-- 'modelVersion', 'detectPHIResponse_modelVersion' - The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
newDetectPHIResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelVersion'
  Prelude.Text ->
  DetectPHIResponse
newDetectPHIResponse pHttpStatus_ pModelVersion_ =
  DetectPHIResponse'
    { paginationToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty,
      modelVersion = pModelVersion_
    }

-- | If the result of the previous request to @DetectPHI@ was truncated,
-- include the @PaginationToken@ to fetch the next page of PHI entities.
detectPHIResponse_paginationToken :: Lens.Lens' DetectPHIResponse (Prelude.Maybe Prelude.Text)
detectPHIResponse_paginationToken = Lens.lens (\DetectPHIResponse' {paginationToken} -> paginationToken) (\s@DetectPHIResponse' {} a -> s {paginationToken = a} :: DetectPHIResponse)

-- | The response's http status code.
detectPHIResponse_httpStatus :: Lens.Lens' DetectPHIResponse Prelude.Int
detectPHIResponse_httpStatus = Lens.lens (\DetectPHIResponse' {httpStatus} -> httpStatus) (\s@DetectPHIResponse' {} a -> s {httpStatus = a} :: DetectPHIResponse)

-- | The collection of PHI entities extracted from the input text and their
-- associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence that Comprehend Medical; has in its
-- detection.
detectPHIResponse_entities :: Lens.Lens' DetectPHIResponse [Entity]
detectPHIResponse_entities = Lens.lens (\DetectPHIResponse' {entities} -> entities) (\s@DetectPHIResponse' {} a -> s {entities = a} :: DetectPHIResponse) Prelude.. Lens.coerced

-- | The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
detectPHIResponse_modelVersion :: Lens.Lens' DetectPHIResponse Prelude.Text
detectPHIResponse_modelVersion = Lens.lens (\DetectPHIResponse' {modelVersion} -> modelVersion) (\s@DetectPHIResponse' {} a -> s {modelVersion = a} :: DetectPHIResponse)

instance Prelude.NFData DetectPHIResponse where
  rnf DetectPHIResponse' {..} =
    Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf modelVersion
