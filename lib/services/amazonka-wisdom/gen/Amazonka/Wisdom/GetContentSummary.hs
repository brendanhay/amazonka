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
-- Module      : Amazonka.Wisdom.GetContentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves summary information about the content.
module Amazonka.Wisdom.GetContentSummary
  ( -- * Creating a Request
    GetContentSummary (..),
    newGetContentSummary,

    -- * Request Lenses
    getContentSummary_contentId,
    getContentSummary_knowledgeBaseId,

    -- * Destructuring the Response
    GetContentSummaryResponse (..),
    newGetContentSummaryResponse,

    -- * Response Lenses
    getContentSummaryResponse_contentSummary,
    getContentSummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetContentSummary' smart constructor.
data GetContentSummary = GetContentSummary'
  { -- | The identifier of the content. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    contentId :: Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentId', 'getContentSummary_contentId' - The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
--
-- 'knowledgeBaseId', 'getContentSummary_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newGetContentSummary ::
  -- | 'contentId'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  GetContentSummary
newGetContentSummary pContentId_ pKnowledgeBaseId_ =
  GetContentSummary'
    { contentId = pContentId_,
      knowledgeBaseId = pKnowledgeBaseId_
    }

-- | The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
getContentSummary_contentId :: Lens.Lens' GetContentSummary Prelude.Text
getContentSummary_contentId = Lens.lens (\GetContentSummary' {contentId} -> contentId) (\s@GetContentSummary' {} a -> s {contentId = a} :: GetContentSummary)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getContentSummary_knowledgeBaseId :: Lens.Lens' GetContentSummary Prelude.Text
getContentSummary_knowledgeBaseId = Lens.lens (\GetContentSummary' {knowledgeBaseId} -> knowledgeBaseId) (\s@GetContentSummary' {} a -> s {knowledgeBaseId = a} :: GetContentSummary)

instance Core.AWSRequest GetContentSummary where
  type
    AWSResponse GetContentSummary =
      GetContentSummaryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContentSummaryResponse'
            Prelude.<$> (x Data..?> "contentSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContentSummary where
  hashWithSalt _salt GetContentSummary' {..} =
    _salt `Prelude.hashWithSalt` contentId
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData GetContentSummary where
  rnf GetContentSummary' {..} =
    Prelude.rnf contentId
      `Prelude.seq` Prelude.rnf knowledgeBaseId

instance Data.ToHeaders GetContentSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetContentSummary where
  toPath GetContentSummary' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/contents/",
        Data.toBS contentId,
        "/summary"
      ]

instance Data.ToQuery GetContentSummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContentSummaryResponse' smart constructor.
data GetContentSummaryResponse = GetContentSummaryResponse'
  { -- | The content summary.
    contentSummary :: Prelude.Maybe ContentSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContentSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentSummary', 'getContentSummaryResponse_contentSummary' - The content summary.
--
-- 'httpStatus', 'getContentSummaryResponse_httpStatus' - The response's http status code.
newGetContentSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContentSummaryResponse
newGetContentSummaryResponse pHttpStatus_ =
  GetContentSummaryResponse'
    { contentSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content summary.
getContentSummaryResponse_contentSummary :: Lens.Lens' GetContentSummaryResponse (Prelude.Maybe ContentSummary)
getContentSummaryResponse_contentSummary = Lens.lens (\GetContentSummaryResponse' {contentSummary} -> contentSummary) (\s@GetContentSummaryResponse' {} a -> s {contentSummary = a} :: GetContentSummaryResponse)

-- | The response's http status code.
getContentSummaryResponse_httpStatus :: Lens.Lens' GetContentSummaryResponse Prelude.Int
getContentSummaryResponse_httpStatus = Lens.lens (\GetContentSummaryResponse' {httpStatus} -> httpStatus) (\s@GetContentSummaryResponse' {} a -> s {httpStatus = a} :: GetContentSummaryResponse)

instance Prelude.NFData GetContentSummaryResponse where
  rnf GetContentSummaryResponse' {..} =
    Prelude.rnf contentSummary
      `Prelude.seq` Prelude.rnf httpStatus
