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
-- Module      : Amazonka.ComprehendMedical.DetectEntitiesV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the clinical text for a variety of medical entities and returns
-- specific information about them such as entity category, location, and
-- confidence score on that information. Amazon Comprehend Medical only
-- detects medical entities in English language texts.
--
-- The @DetectEntitiesV2@ operation replaces the DetectEntities operation.
-- This new action uses a different model for determining the entities in
-- your medical text and changes the way that some entities are returned in
-- the output. You should use the @DetectEntitiesV2@ operation in all new
-- applications.
--
-- The @DetectEntitiesV2@ operation returns the @Acuity@ and @Direction@
-- entities as attributes instead of types.
module Amazonka.ComprehendMedical.DetectEntitiesV2
  ( -- * Creating a Request
    DetectEntitiesV2 (..),
    newDetectEntitiesV2,

    -- * Request Lenses
    detectEntitiesV2_text,

    -- * Destructuring the Response
    DetectEntitiesV2Response (..),
    newDetectEntitiesV2Response,

    -- * Response Lenses
    detectEntitiesV2Response_paginationToken,
    detectEntitiesV2Response_unmappedAttributes,
    detectEntitiesV2Response_httpStatus,
    detectEntitiesV2Response_entities,
    detectEntitiesV2Response_modelVersion,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectEntitiesV2' smart constructor.
data DetectEntitiesV2 = DetectEntitiesV2'
  { -- | A UTF-8 string containing the clinical content being examined for
    -- entities. Each string must contain fewer than 20,000 bytes of
    -- characters.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectEntitiesV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectEntitiesV2_text' - A UTF-8 string containing the clinical content being examined for
-- entities. Each string must contain fewer than 20,000 bytes of
-- characters.
newDetectEntitiesV2 ::
  -- | 'text'
  Prelude.Text ->
  DetectEntitiesV2
newDetectEntitiesV2 pText_ =
  DetectEntitiesV2' {text = pText_}

-- | A UTF-8 string containing the clinical content being examined for
-- entities. Each string must contain fewer than 20,000 bytes of
-- characters.
detectEntitiesV2_text :: Lens.Lens' DetectEntitiesV2 Prelude.Text
detectEntitiesV2_text = Lens.lens (\DetectEntitiesV2' {text} -> text) (\s@DetectEntitiesV2' {} a -> s {text = a} :: DetectEntitiesV2)

instance Core.AWSRequest DetectEntitiesV2 where
  type
    AWSResponse DetectEntitiesV2 =
      DetectEntitiesV2Response
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectEntitiesV2Response'
            Prelude.<$> (x Data..?> "PaginationToken")
            Prelude.<*> ( x
                            Data..?> "UnmappedAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "ModelVersion")
      )

instance Prelude.Hashable DetectEntitiesV2 where
  hashWithSalt _salt DetectEntitiesV2' {..} =
    _salt `Prelude.hashWithSalt` text

instance Prelude.NFData DetectEntitiesV2 where
  rnf DetectEntitiesV2' {..} = Prelude.rnf text

instance Data.ToHeaders DetectEntitiesV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.DetectEntitiesV2" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectEntitiesV2 where
  toJSON DetectEntitiesV2' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Text" Data..= text)]
      )

instance Data.ToPath DetectEntitiesV2 where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectEntitiesV2 where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectEntitiesV2Response' smart constructor.
data DetectEntitiesV2Response = DetectEntitiesV2Response'
  { -- | If the result to the @DetectEntitiesV2@ operation was truncated, include
    -- the @PaginationToken@ to fetch the next page of entities.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | Attributes extracted from the input text that couldn\'t be related to an
    -- entity.
    unmappedAttributes :: Prelude.Maybe [UnmappedAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The collection of medical entities extracted from the input text and
    -- their associated information. For each entity, the response provides the
    -- entity text, the entity category, where the entity text begins and ends,
    -- and the level of confidence in the detection and analysis. Attributes
    -- and traits of the entity are also returned.
    entities :: [Entity],
    -- | The version of the model used to analyze the documents. The version
    -- number looks like X.X.X. You can use this information to track the model
    -- used for a particular batch of documents.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectEntitiesV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'detectEntitiesV2Response_paginationToken' - If the result to the @DetectEntitiesV2@ operation was truncated, include
-- the @PaginationToken@ to fetch the next page of entities.
--
-- 'unmappedAttributes', 'detectEntitiesV2Response_unmappedAttributes' - Attributes extracted from the input text that couldn\'t be related to an
-- entity.
--
-- 'httpStatus', 'detectEntitiesV2Response_httpStatus' - The response's http status code.
--
-- 'entities', 'detectEntitiesV2Response_entities' - The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence in the detection and analysis. Attributes
-- and traits of the entity are also returned.
--
-- 'modelVersion', 'detectEntitiesV2Response_modelVersion' - The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
newDetectEntitiesV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'modelVersion'
  Prelude.Text ->
  DetectEntitiesV2Response
newDetectEntitiesV2Response
  pHttpStatus_
  pModelVersion_ =
    DetectEntitiesV2Response'
      { paginationToken =
          Prelude.Nothing,
        unmappedAttributes = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        entities = Prelude.mempty,
        modelVersion = pModelVersion_
      }

-- | If the result to the @DetectEntitiesV2@ operation was truncated, include
-- the @PaginationToken@ to fetch the next page of entities.
detectEntitiesV2Response_paginationToken :: Lens.Lens' DetectEntitiesV2Response (Prelude.Maybe Prelude.Text)
detectEntitiesV2Response_paginationToken = Lens.lens (\DetectEntitiesV2Response' {paginationToken} -> paginationToken) (\s@DetectEntitiesV2Response' {} a -> s {paginationToken = a} :: DetectEntitiesV2Response)

-- | Attributes extracted from the input text that couldn\'t be related to an
-- entity.
detectEntitiesV2Response_unmappedAttributes :: Lens.Lens' DetectEntitiesV2Response (Prelude.Maybe [UnmappedAttribute])
detectEntitiesV2Response_unmappedAttributes = Lens.lens (\DetectEntitiesV2Response' {unmappedAttributes} -> unmappedAttributes) (\s@DetectEntitiesV2Response' {} a -> s {unmappedAttributes = a} :: DetectEntitiesV2Response) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectEntitiesV2Response_httpStatus :: Lens.Lens' DetectEntitiesV2Response Prelude.Int
detectEntitiesV2Response_httpStatus = Lens.lens (\DetectEntitiesV2Response' {httpStatus} -> httpStatus) (\s@DetectEntitiesV2Response' {} a -> s {httpStatus = a} :: DetectEntitiesV2Response)

-- | The collection of medical entities extracted from the input text and
-- their associated information. For each entity, the response provides the
-- entity text, the entity category, where the entity text begins and ends,
-- and the level of confidence in the detection and analysis. Attributes
-- and traits of the entity are also returned.
detectEntitiesV2Response_entities :: Lens.Lens' DetectEntitiesV2Response [Entity]
detectEntitiesV2Response_entities = Lens.lens (\DetectEntitiesV2Response' {entities} -> entities) (\s@DetectEntitiesV2Response' {} a -> s {entities = a} :: DetectEntitiesV2Response) Prelude.. Lens.coerced

-- | The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
detectEntitiesV2Response_modelVersion :: Lens.Lens' DetectEntitiesV2Response Prelude.Text
detectEntitiesV2Response_modelVersion = Lens.lens (\DetectEntitiesV2Response' {modelVersion} -> modelVersion) (\s@DetectEntitiesV2Response' {} a -> s {modelVersion = a} :: DetectEntitiesV2Response)

instance Prelude.NFData DetectEntitiesV2Response where
  rnf DetectEntitiesV2Response' {..} =
    Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf unmappedAttributes
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf modelVersion
