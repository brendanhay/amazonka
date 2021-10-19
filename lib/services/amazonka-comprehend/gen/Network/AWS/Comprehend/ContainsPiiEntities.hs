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
-- Module      : Network.AWS.Comprehend.ContainsPiiEntities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Analyzes input text for the presence of personally identifiable
-- information (PII) and returns the labels of identified PII entity types
-- such as name, address, bank account number, or phone number.
module Network.AWS.Comprehend.ContainsPiiEntities
  ( -- * Creating a Request
    ContainsPiiEntities (..),
    newContainsPiiEntities,

    -- * Request Lenses
    containsPiiEntities_text,
    containsPiiEntities_languageCode,

    -- * Destructuring the Response
    ContainsPiiEntitiesResponse (..),
    newContainsPiiEntitiesResponse,

    -- * Response Lenses
    containsPiiEntitiesResponse_labels,
    containsPiiEntitiesResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newContainsPiiEntities' smart constructor.
data ContainsPiiEntities = ContainsPiiEntities'
  { -- | Creates a new document classification request to analyze a single
    -- document in real-time, returning personally identifiable information
    -- (PII) entity labels.
    text :: Prelude.Text,
    -- | The language of the input documents.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainsPiiEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'containsPiiEntities_text' - Creates a new document classification request to analyze a single
-- document in real-time, returning personally identifiable information
-- (PII) entity labels.
--
-- 'languageCode', 'containsPiiEntities_languageCode' - The language of the input documents.
newContainsPiiEntities ::
  -- | 'text'
  Prelude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  ContainsPiiEntities
newContainsPiiEntities pText_ pLanguageCode_ =
  ContainsPiiEntities'
    { text = pText_,
      languageCode = pLanguageCode_
    }

-- | Creates a new document classification request to analyze a single
-- document in real-time, returning personally identifiable information
-- (PII) entity labels.
containsPiiEntities_text :: Lens.Lens' ContainsPiiEntities Prelude.Text
containsPiiEntities_text = Lens.lens (\ContainsPiiEntities' {text} -> text) (\s@ContainsPiiEntities' {} a -> s {text = a} :: ContainsPiiEntities)

-- | The language of the input documents.
containsPiiEntities_languageCode :: Lens.Lens' ContainsPiiEntities LanguageCode
containsPiiEntities_languageCode = Lens.lens (\ContainsPiiEntities' {languageCode} -> languageCode) (\s@ContainsPiiEntities' {} a -> s {languageCode = a} :: ContainsPiiEntities)

instance Core.AWSRequest ContainsPiiEntities where
  type
    AWSResponse ContainsPiiEntities =
      ContainsPiiEntitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ContainsPiiEntitiesResponse'
            Prelude.<$> (x Core..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ContainsPiiEntities

instance Prelude.NFData ContainsPiiEntities

instance Core.ToHeaders ContainsPiiEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ContainsPiiEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ContainsPiiEntities where
  toJSON ContainsPiiEntities' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Core..= text),
            Prelude.Just ("LanguageCode" Core..= languageCode)
          ]
      )

instance Core.ToPath ContainsPiiEntities where
  toPath = Prelude.const "/"

instance Core.ToQuery ContainsPiiEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newContainsPiiEntitiesResponse' smart constructor.
data ContainsPiiEntitiesResponse = ContainsPiiEntitiesResponse'
  { -- | The labels used in the document being analyzed. Individual labels
    -- represent personally identifiable information (PII) entity types.
    labels :: Prelude.Maybe [EntityLabel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainsPiiEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'containsPiiEntitiesResponse_labels' - The labels used in the document being analyzed. Individual labels
-- represent personally identifiable information (PII) entity types.
--
-- 'httpStatus', 'containsPiiEntitiesResponse_httpStatus' - The response's http status code.
newContainsPiiEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ContainsPiiEntitiesResponse
newContainsPiiEntitiesResponse pHttpStatus_ =
  ContainsPiiEntitiesResponse'
    { labels =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The labels used in the document being analyzed. Individual labels
-- represent personally identifiable information (PII) entity types.
containsPiiEntitiesResponse_labels :: Lens.Lens' ContainsPiiEntitiesResponse (Prelude.Maybe [EntityLabel])
containsPiiEntitiesResponse_labels = Lens.lens (\ContainsPiiEntitiesResponse' {labels} -> labels) (\s@ContainsPiiEntitiesResponse' {} a -> s {labels = a} :: ContainsPiiEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
containsPiiEntitiesResponse_httpStatus :: Lens.Lens' ContainsPiiEntitiesResponse Prelude.Int
containsPiiEntitiesResponse_httpStatus = Lens.lens (\ContainsPiiEntitiesResponse' {httpStatus} -> httpStatus) (\s@ContainsPiiEntitiesResponse' {} a -> s {httpStatus = a} :: ContainsPiiEntitiesResponse)

instance Prelude.NFData ContainsPiiEntitiesResponse
