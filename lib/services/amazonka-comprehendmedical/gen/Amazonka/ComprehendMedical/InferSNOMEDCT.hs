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
-- Module      : Amazonka.ComprehendMedical.InferSNOMEDCT
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- InferSNOMEDCT detects possible medical concepts as entities and links
-- them to codes from the Systematized Nomenclature of Medicine, Clinical
-- Terms (SNOMED-CT) ontology
module Amazonka.ComprehendMedical.InferSNOMEDCT
  ( -- * Creating a Request
    InferSNOMEDCT (..),
    newInferSNOMEDCT,

    -- * Request Lenses
    inferSNOMEDCT_text,

    -- * Destructuring the Response
    InferSNOMEDCTResponse (..),
    newInferSNOMEDCTResponse,

    -- * Response Lenses
    inferSNOMEDCTResponse_characters,
    inferSNOMEDCTResponse_modelVersion,
    inferSNOMEDCTResponse_paginationToken,
    inferSNOMEDCTResponse_sNOMEDCTDetails,
    inferSNOMEDCTResponse_httpStatus,
    inferSNOMEDCTResponse_entities,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInferSNOMEDCT' smart constructor.
data InferSNOMEDCT = InferSNOMEDCT'
  { -- | The input text to be analyzed using InferSNOMEDCT. The text should be a
    -- string with 1 to 10000 characters.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferSNOMEDCT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'inferSNOMEDCT_text' - The input text to be analyzed using InferSNOMEDCT. The text should be a
-- string with 1 to 10000 characters.
newInferSNOMEDCT ::
  -- | 'text'
  Prelude.Text ->
  InferSNOMEDCT
newInferSNOMEDCT pText_ =
  InferSNOMEDCT' {text = pText_}

-- | The input text to be analyzed using InferSNOMEDCT. The text should be a
-- string with 1 to 10000 characters.
inferSNOMEDCT_text :: Lens.Lens' InferSNOMEDCT Prelude.Text
inferSNOMEDCT_text = Lens.lens (\InferSNOMEDCT' {text} -> text) (\s@InferSNOMEDCT' {} a -> s {text = a} :: InferSNOMEDCT)

instance Core.AWSRequest InferSNOMEDCT where
  type
    AWSResponse InferSNOMEDCT =
      InferSNOMEDCTResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InferSNOMEDCTResponse'
            Prelude.<$> (x Data..?> "Characters")
            Prelude.<*> (x Data..?> "ModelVersion")
            Prelude.<*> (x Data..?> "PaginationToken")
            Prelude.<*> (x Data..?> "SNOMEDCTDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable InferSNOMEDCT where
  hashWithSalt _salt InferSNOMEDCT' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData InferSNOMEDCT where
  rnf InferSNOMEDCT' {..} = Prelude.rnf text

instance Data.ToHeaders InferSNOMEDCT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.InferSNOMEDCT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InferSNOMEDCT where
  toJSON InferSNOMEDCT' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Text" Data..= text)]
      )

instance Data.ToPath InferSNOMEDCT where
  toPath = Prelude.const "/"

instance Data.ToQuery InferSNOMEDCT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInferSNOMEDCTResponse' smart constructor.
data InferSNOMEDCTResponse = InferSNOMEDCTResponse'
  { -- | The number of characters in the input request documentation.
    characters :: Prelude.Maybe Characters,
    -- | The version of the model used to analyze the documents, in the format
    -- n.n.n You can use this information to track the model used for a
    -- particular batch of documents.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | If the result of the request is truncated, the pagination token can be
    -- used to fetch the next page of entities.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The details of the SNOMED-CT revision, including the edition, language,
    -- and version date.
    sNOMEDCTDetails :: Prelude.Maybe SNOMEDCTDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The collection of medical concept entities extracted from the input text
    -- and their associated information. For each entity, the response provides
    -- the entity text, the entity category, where the entity text begins and
    -- ends, and the level of confidence that Comprehend Medical has in the
    -- detection and analysis. Attributes and traits of the entity are also
    -- returned.
    entities :: [SNOMEDCTEntity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferSNOMEDCTResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'characters', 'inferSNOMEDCTResponse_characters' - The number of characters in the input request documentation.
--
-- 'modelVersion', 'inferSNOMEDCTResponse_modelVersion' - The version of the model used to analyze the documents, in the format
-- n.n.n You can use this information to track the model used for a
-- particular batch of documents.
--
-- 'paginationToken', 'inferSNOMEDCTResponse_paginationToken' - If the result of the request is truncated, the pagination token can be
-- used to fetch the next page of entities.
--
-- 'sNOMEDCTDetails', 'inferSNOMEDCTResponse_sNOMEDCTDetails' - The details of the SNOMED-CT revision, including the edition, language,
-- and version date.
--
-- 'httpStatus', 'inferSNOMEDCTResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'inferSNOMEDCTResponse_entities' - The collection of medical concept entities extracted from the input text
-- and their associated information. For each entity, the response provides
-- the entity text, the entity category, where the entity text begins and
-- ends, and the level of confidence that Comprehend Medical has in the
-- detection and analysis. Attributes and traits of the entity are also
-- returned.
newInferSNOMEDCTResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InferSNOMEDCTResponse
newInferSNOMEDCTResponse pHttpStatus_ =
  InferSNOMEDCTResponse'
    { characters =
        Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      paginationToken = Prelude.Nothing,
      sNOMEDCTDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The number of characters in the input request documentation.
inferSNOMEDCTResponse_characters :: Lens.Lens' InferSNOMEDCTResponse (Prelude.Maybe Characters)
inferSNOMEDCTResponse_characters = Lens.lens (\InferSNOMEDCTResponse' {characters} -> characters) (\s@InferSNOMEDCTResponse' {} a -> s {characters = a} :: InferSNOMEDCTResponse)

-- | The version of the model used to analyze the documents, in the format
-- n.n.n You can use this information to track the model used for a
-- particular batch of documents.
inferSNOMEDCTResponse_modelVersion :: Lens.Lens' InferSNOMEDCTResponse (Prelude.Maybe Prelude.Text)
inferSNOMEDCTResponse_modelVersion = Lens.lens (\InferSNOMEDCTResponse' {modelVersion} -> modelVersion) (\s@InferSNOMEDCTResponse' {} a -> s {modelVersion = a} :: InferSNOMEDCTResponse)

-- | If the result of the request is truncated, the pagination token can be
-- used to fetch the next page of entities.
inferSNOMEDCTResponse_paginationToken :: Lens.Lens' InferSNOMEDCTResponse (Prelude.Maybe Prelude.Text)
inferSNOMEDCTResponse_paginationToken = Lens.lens (\InferSNOMEDCTResponse' {paginationToken} -> paginationToken) (\s@InferSNOMEDCTResponse' {} a -> s {paginationToken = a} :: InferSNOMEDCTResponse)

-- | The details of the SNOMED-CT revision, including the edition, language,
-- and version date.
inferSNOMEDCTResponse_sNOMEDCTDetails :: Lens.Lens' InferSNOMEDCTResponse (Prelude.Maybe SNOMEDCTDetails)
inferSNOMEDCTResponse_sNOMEDCTDetails = Lens.lens (\InferSNOMEDCTResponse' {sNOMEDCTDetails} -> sNOMEDCTDetails) (\s@InferSNOMEDCTResponse' {} a -> s {sNOMEDCTDetails = a} :: InferSNOMEDCTResponse)

-- | The response's http status code.
inferSNOMEDCTResponse_httpStatus :: Lens.Lens' InferSNOMEDCTResponse Prelude.Int
inferSNOMEDCTResponse_httpStatus = Lens.lens (\InferSNOMEDCTResponse' {httpStatus} -> httpStatus) (\s@InferSNOMEDCTResponse' {} a -> s {httpStatus = a} :: InferSNOMEDCTResponse)

-- | The collection of medical concept entities extracted from the input text
-- and their associated information. For each entity, the response provides
-- the entity text, the entity category, where the entity text begins and
-- ends, and the level of confidence that Comprehend Medical has in the
-- detection and analysis. Attributes and traits of the entity are also
-- returned.
inferSNOMEDCTResponse_entities :: Lens.Lens' InferSNOMEDCTResponse [SNOMEDCTEntity]
inferSNOMEDCTResponse_entities = Lens.lens (\InferSNOMEDCTResponse' {entities} -> entities) (\s@InferSNOMEDCTResponse' {} a -> s {entities = a} :: InferSNOMEDCTResponse) Prelude.. Lens.coerced

instance Prelude.NFData InferSNOMEDCTResponse where
  rnf InferSNOMEDCTResponse' {..} =
    Prelude.rnf characters
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf sNOMEDCTDetails
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
