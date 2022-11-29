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
-- Module      : Amazonka.Comprehend.DetectPiiEntities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects the input text for entities that contain personally
-- identifiable information (PII) and returns information about them.
module Amazonka.Comprehend.DetectPiiEntities
  ( -- * Creating a Request
    DetectPiiEntities (..),
    newDetectPiiEntities,

    -- * Request Lenses
    detectPiiEntities_text,
    detectPiiEntities_languageCode,

    -- * Destructuring the Response
    DetectPiiEntitiesResponse (..),
    newDetectPiiEntitiesResponse,

    -- * Response Lenses
    detectPiiEntitiesResponse_entities,
    detectPiiEntitiesResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectPiiEntities' smart constructor.
data DetectPiiEntities = DetectPiiEntities'
  { -- | A UTF-8 text string. The maximum string size is 100 KB.
    text :: Prelude.Text,
    -- | The language of the input documents. Currently, English is the only
    -- valid language.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectPiiEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'detectPiiEntities_text' - A UTF-8 text string. The maximum string size is 100 KB.
--
-- 'languageCode', 'detectPiiEntities_languageCode' - The language of the input documents. Currently, English is the only
-- valid language.
newDetectPiiEntities ::
  -- | 'text'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  DetectPiiEntities
newDetectPiiEntities pText_ pLanguageCode_ =
  DetectPiiEntities'
    { text = pText_,
      languageCode = pLanguageCode_
    }

-- | A UTF-8 text string. The maximum string size is 100 KB.
detectPiiEntities_text :: Lens.Lens' DetectPiiEntities Prelude.Text
detectPiiEntities_text = Lens.lens (\DetectPiiEntities' {text} -> text) (\s@DetectPiiEntities' {} a -> s {text = a} :: DetectPiiEntities)

-- | The language of the input documents. Currently, English is the only
-- valid language.
detectPiiEntities_languageCode :: Lens.Lens' DetectPiiEntities LanguageCode
detectPiiEntities_languageCode = Lens.lens (\DetectPiiEntities' {languageCode} -> languageCode) (\s@DetectPiiEntities' {} a -> s {languageCode = a} :: DetectPiiEntities)

instance Core.AWSRequest DetectPiiEntities where
  type
    AWSResponse DetectPiiEntities =
      DetectPiiEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectPiiEntitiesResponse'
            Prelude.<$> (x Core..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectPiiEntities where
  hashWithSalt _salt DetectPiiEntities' {..} =
    _salt `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData DetectPiiEntities where
  rnf DetectPiiEntities' {..} =
    Prelude.rnf text
      `Prelude.seq` Prelude.rnf languageCode

instance Core.ToHeaders DetectPiiEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectPiiEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectPiiEntities where
  toJSON DetectPiiEntities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Core..= text),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath DetectPiiEntities where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectPiiEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectPiiEntitiesResponse' smart constructor.
data DetectPiiEntitiesResponse = DetectPiiEntitiesResponse'
  { -- | A collection of PII entities identified in the input text. For each
    -- entity, the response provides the entity type, where the entity text
    -- begins and ends, and the level of confidence that Amazon Comprehend has
    -- in the detection.
    entities :: Prelude.Maybe [PiiEntity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectPiiEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entities', 'detectPiiEntitiesResponse_entities' - A collection of PII entities identified in the input text. For each
-- entity, the response provides the entity type, where the entity text
-- begins and ends, and the level of confidence that Amazon Comprehend has
-- in the detection.
--
-- 'httpStatus', 'detectPiiEntitiesResponse_httpStatus' - The response's http status code.
newDetectPiiEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectPiiEntitiesResponse
newDetectPiiEntitiesResponse pHttpStatus_ =
  DetectPiiEntitiesResponse'
    { entities =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of PII entities identified in the input text. For each
-- entity, the response provides the entity type, where the entity text
-- begins and ends, and the level of confidence that Amazon Comprehend has
-- in the detection.
detectPiiEntitiesResponse_entities :: Lens.Lens' DetectPiiEntitiesResponse (Prelude.Maybe [PiiEntity])
detectPiiEntitiesResponse_entities = Lens.lens (\DetectPiiEntitiesResponse' {entities} -> entities) (\s@DetectPiiEntitiesResponse' {} a -> s {entities = a} :: DetectPiiEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectPiiEntitiesResponse_httpStatus :: Lens.Lens' DetectPiiEntitiesResponse Prelude.Int
detectPiiEntitiesResponse_httpStatus = Lens.lens (\DetectPiiEntitiesResponse' {httpStatus} -> httpStatus) (\s@DetectPiiEntitiesResponse' {} a -> s {httpStatus = a} :: DetectPiiEntitiesResponse)

instance Prelude.NFData DetectPiiEntitiesResponse where
  rnf DetectPiiEntitiesResponse' {..} =
    Prelude.rnf entities
      `Prelude.seq` Prelude.rnf httpStatus
