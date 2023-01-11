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
-- Module      : Amazonka.Wisdom.GetContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves content, including a pre-signed URL to download the content.
module Amazonka.Wisdom.GetContent
  ( -- * Creating a Request
    GetContent (..),
    newGetContent,

    -- * Request Lenses
    getContent_contentId,
    getContent_knowledgeBaseId,

    -- * Destructuring the Response
    GetContentResponse (..),
    newGetContentResponse,

    -- * Response Lenses
    getContentResponse_content,
    getContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newGetContent' smart constructor.
data GetContent = GetContent'
  { -- | The identifier of the content. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    contentId :: Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentId', 'getContent_contentId' - The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
--
-- 'knowledgeBaseId', 'getContent_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newGetContent ::
  -- | 'contentId'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  GetContent
newGetContent pContentId_ pKnowledgeBaseId_ =
  GetContent'
    { contentId = pContentId_,
      knowledgeBaseId = pKnowledgeBaseId_
    }

-- | The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
getContent_contentId :: Lens.Lens' GetContent Prelude.Text
getContent_contentId = Lens.lens (\GetContent' {contentId} -> contentId) (\s@GetContent' {} a -> s {contentId = a} :: GetContent)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
getContent_knowledgeBaseId :: Lens.Lens' GetContent Prelude.Text
getContent_knowledgeBaseId = Lens.lens (\GetContent' {knowledgeBaseId} -> knowledgeBaseId) (\s@GetContent' {} a -> s {knowledgeBaseId = a} :: GetContent)

instance Core.AWSRequest GetContent where
  type AWSResponse GetContent = GetContentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContentResponse'
            Prelude.<$> (x Data..?> "content")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContent where
  hashWithSalt _salt GetContent' {..} =
    _salt `Prelude.hashWithSalt` contentId
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData GetContent where
  rnf GetContent' {..} =
    Prelude.rnf contentId
      `Prelude.seq` Prelude.rnf knowledgeBaseId

instance Data.ToHeaders GetContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetContent where
  toPath GetContent' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/contents/",
        Data.toBS contentId
      ]

instance Data.ToQuery GetContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContentResponse' smart constructor.
data GetContentResponse = GetContentResponse'
  { -- | The content.
    content :: Prelude.Maybe ContentData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'getContentResponse_content' - The content.
--
-- 'httpStatus', 'getContentResponse_httpStatus' - The response's http status code.
newGetContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContentResponse
newGetContentResponse pHttpStatus_ =
  GetContentResponse'
    { content = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content.
getContentResponse_content :: Lens.Lens' GetContentResponse (Prelude.Maybe ContentData)
getContentResponse_content = Lens.lens (\GetContentResponse' {content} -> content) (\s@GetContentResponse' {} a -> s {content = a} :: GetContentResponse)

-- | The response's http status code.
getContentResponse_httpStatus :: Lens.Lens' GetContentResponse Prelude.Int
getContentResponse_httpStatus = Lens.lens (\GetContentResponse' {httpStatus} -> httpStatus) (\s@GetContentResponse' {} a -> s {httpStatus = a} :: GetContentResponse)

instance Prelude.NFData GetContentResponse where
  rnf GetContentResponse' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf httpStatus
