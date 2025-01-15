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
-- Module      : Amazonka.Wisdom.DeleteContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content.
module Amazonka.Wisdom.DeleteContent
  ( -- * Creating a Request
    DeleteContent (..),
    newDeleteContent,

    -- * Request Lenses
    deleteContent_contentId,
    deleteContent_knowledgeBaseId,

    -- * Destructuring the Response
    DeleteContentResponse (..),
    newDeleteContentResponse,

    -- * Response Lenses
    deleteContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newDeleteContent' smart constructor.
data DeleteContent = DeleteContent'
  { -- | The identifier of the content. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    contentId :: Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentId', 'deleteContent_contentId' - The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
--
-- 'knowledgeBaseId', 'deleteContent_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newDeleteContent ::
  -- | 'contentId'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  DeleteContent
newDeleteContent pContentId_ pKnowledgeBaseId_ =
  DeleteContent'
    { contentId = pContentId_,
      knowledgeBaseId = pKnowledgeBaseId_
    }

-- | The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
deleteContent_contentId :: Lens.Lens' DeleteContent Prelude.Text
deleteContent_contentId = Lens.lens (\DeleteContent' {contentId} -> contentId) (\s@DeleteContent' {} a -> s {contentId = a} :: DeleteContent)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
deleteContent_knowledgeBaseId :: Lens.Lens' DeleteContent Prelude.Text
deleteContent_knowledgeBaseId = Lens.lens (\DeleteContent' {knowledgeBaseId} -> knowledgeBaseId) (\s@DeleteContent' {} a -> s {knowledgeBaseId = a} :: DeleteContent)

instance Core.AWSRequest DeleteContent where
  type
    AWSResponse DeleteContent =
      DeleteContentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContent where
  hashWithSalt _salt DeleteContent' {..} =
    _salt
      `Prelude.hashWithSalt` contentId
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData DeleteContent where
  rnf DeleteContent' {..} =
    Prelude.rnf contentId `Prelude.seq`
      Prelude.rnf knowledgeBaseId

instance Data.ToHeaders DeleteContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteContent where
  toPath DeleteContent' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/contents/",
        Data.toBS contentId
      ]

instance Data.ToQuery DeleteContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContentResponse' smart constructor.
data DeleteContentResponse = DeleteContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContentResponse_httpStatus' - The response's http status code.
newDeleteContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContentResponse
newDeleteContentResponse pHttpStatus_ =
  DeleteContentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteContentResponse_httpStatus :: Lens.Lens' DeleteContentResponse Prelude.Int
deleteContentResponse_httpStatus = Lens.lens (\DeleteContentResponse' {httpStatus} -> httpStatus) (\s@DeleteContentResponse' {} a -> s {httpStatus = a} :: DeleteContentResponse)

instance Prelude.NFData DeleteContentResponse where
  rnf DeleteContentResponse' {..} =
    Prelude.rnf httpStatus
