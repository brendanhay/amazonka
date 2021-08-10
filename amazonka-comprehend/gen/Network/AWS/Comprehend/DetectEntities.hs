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
-- Module      : Network.AWS.Comprehend.DetectEntities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inspects text for named entities, and returns information about them.
-- For more information, about named entities, see how-entities.
module Network.AWS.Comprehend.DetectEntities
  ( -- * Creating a Request
    DetectEntities (..),
    newDetectEntities,

    -- * Request Lenses
    detectEntities_languageCode,
    detectEntities_endpointArn,
    detectEntities_text,

    -- * Destructuring the Response
    DetectEntitiesResponse (..),
    newDetectEntitiesResponse,

    -- * Response Lenses
    detectEntitiesResponse_entities,
    detectEntitiesResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetectEntities' smart constructor.
data DetectEntities = DetectEntities'
  { -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. All documents must be in the
    -- same language.
    --
    -- If your request includes the endpoint for a custom entity recognition
    -- model, Amazon Comprehend uses the language of your custom model, and it
    -- ignores any language code that you specify here.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The Amazon Resource Name of an endpoint that is associated with a custom
    -- entity recognition model. Provide an endpoint if you want to detect
    -- entities by using your own custom model instead of the default model
    -- that is used by Amazon Comprehend.
    --
    -- If you specify an endpoint, Amazon Comprehend uses the language of your
    -- custom model, and it ignores any language code that you provide in your
    -- request.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
    -- UTF-8 encoded characters.
    text :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'detectEntities_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
--
-- If your request includes the endpoint for a custom entity recognition
-- model, Amazon Comprehend uses the language of your custom model, and it
-- ignores any language code that you specify here.
--
-- 'endpointArn', 'detectEntities_endpointArn' - The Amazon Resource Name of an endpoint that is associated with a custom
-- entity recognition model. Provide an endpoint if you want to detect
-- entities by using your own custom model instead of the default model
-- that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your
-- custom model, and it ignores any language code that you provide in your
-- request.
--
-- 'text', 'detectEntities_text' - A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
newDetectEntities ::
  -- | 'text'
  Prelude.Text ->
  DetectEntities
newDetectEntities pText_ =
  DetectEntities'
    { languageCode = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      text = Core._Sensitive Lens.# pText_
    }

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. All documents must be in the
-- same language.
--
-- If your request includes the endpoint for a custom entity recognition
-- model, Amazon Comprehend uses the language of your custom model, and it
-- ignores any language code that you specify here.
detectEntities_languageCode :: Lens.Lens' DetectEntities (Prelude.Maybe LanguageCode)
detectEntities_languageCode = Lens.lens (\DetectEntities' {languageCode} -> languageCode) (\s@DetectEntities' {} a -> s {languageCode = a} :: DetectEntities)

-- | The Amazon Resource Name of an endpoint that is associated with a custom
-- entity recognition model. Provide an endpoint if you want to detect
-- entities by using your own custom model instead of the default model
-- that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your
-- custom model, and it ignores any language code that you provide in your
-- request.
detectEntities_endpointArn :: Lens.Lens' DetectEntities (Prelude.Maybe Prelude.Text)
detectEntities_endpointArn = Lens.lens (\DetectEntities' {endpointArn} -> endpointArn) (\s@DetectEntities' {} a -> s {endpointArn = a} :: DetectEntities)

-- | A UTF-8 text string. Each string must contain fewer that 5,000 bytes of
-- UTF-8 encoded characters.
detectEntities_text :: Lens.Lens' DetectEntities Prelude.Text
detectEntities_text = Lens.lens (\DetectEntities' {text} -> text) (\s@DetectEntities' {} a -> s {text = a} :: DetectEntities) Prelude.. Core._Sensitive

instance Core.AWSRequest DetectEntities where
  type
    AWSResponse DetectEntities =
      DetectEntitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectEntitiesResponse'
            Prelude.<$> (x Core..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectEntities

instance Prelude.NFData DetectEntities

instance Core.ToHeaders DetectEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DetectEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectEntities where
  toJSON DetectEntities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("EndpointArn" Core..=) Prelude.<$> endpointArn,
            Prelude.Just ("Text" Core..= text)
          ]
      )

instance Core.ToPath DetectEntities where
  toPath = Prelude.const "/"

instance Core.ToQuery DetectEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { -- | A collection of entities identified in the input text. For each entity,
    -- the response provides the entity text, entity type, where the entity
    -- text begins and ends, and the level of confidence that Amazon Comprehend
    -- has in the detection.
    --
    -- If your request uses a custom entity recognition model, Amazon
    -- Comprehend detects the entities that the model is trained to recognize.
    -- Otherwise, it detects the default entity types. For a list of default
    -- entity types, see how-entities.
    entities :: Prelude.Maybe [Entity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entities', 'detectEntitiesResponse_entities' - A collection of entities identified in the input text. For each entity,
-- the response provides the entity text, entity type, where the entity
-- text begins and ends, and the level of confidence that Amazon Comprehend
-- has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon
-- Comprehend detects the entities that the model is trained to recognize.
-- Otherwise, it detects the default entity types. For a list of default
-- entity types, see how-entities.
--
-- 'httpStatus', 'detectEntitiesResponse_httpStatus' - The response's http status code.
newDetectEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectEntitiesResponse
newDetectEntitiesResponse pHttpStatus_ =
  DetectEntitiesResponse'
    { entities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of entities identified in the input text. For each entity,
-- the response provides the entity text, entity type, where the entity
-- text begins and ends, and the level of confidence that Amazon Comprehend
-- has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon
-- Comprehend detects the entities that the model is trained to recognize.
-- Otherwise, it detects the default entity types. For a list of default
-- entity types, see how-entities.
detectEntitiesResponse_entities :: Lens.Lens' DetectEntitiesResponse (Prelude.Maybe [Entity])
detectEntitiesResponse_entities = Lens.lens (\DetectEntitiesResponse' {entities} -> entities) (\s@DetectEntitiesResponse' {} a -> s {entities = a} :: DetectEntitiesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detectEntitiesResponse_httpStatus :: Lens.Lens' DetectEntitiesResponse Prelude.Int
detectEntitiesResponse_httpStatus = Lens.lens (\DetectEntitiesResponse' {httpStatus} -> httpStatus) (\s@DetectEntitiesResponse' {} a -> s {httpStatus = a} :: DetectEntitiesResponse)

instance Prelude.NFData DetectEntitiesResponse
