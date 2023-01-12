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
-- Module      : Amazonka.Wisdom.UpdateContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the content.
module Amazonka.Wisdom.UpdateContent
  ( -- * Creating a Request
    UpdateContent (..),
    newUpdateContent,

    -- * Request Lenses
    updateContent_metadata,
    updateContent_overrideLinkOutUri,
    updateContent_removeOverrideLinkOutUri,
    updateContent_revisionId,
    updateContent_title,
    updateContent_uploadId,
    updateContent_contentId,
    updateContent_knowledgeBaseId,

    -- * Destructuring the Response
    UpdateContentResponse (..),
    newUpdateContentResponse,

    -- * Response Lenses
    updateContentResponse_content,
    updateContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newUpdateContent' smart constructor.
data UpdateContent = UpdateContent'
  { -- | A key\/value map to store attributes without affecting tagging or
    -- recommendations. For example, when synchronizing data between an
    -- external system and Wisdom, you can store an external version identifier
    -- as metadata to utilize for determining drift.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URI for the article. If the knowledge base has a templateUri,
    -- setting this argument overrides it for this piece of content. To remove
    -- an existing @overrideLinkOurUri@, exclude this argument and set
    -- @removeOverrideLinkOutUri@ to true.
    overrideLinkOutUri :: Prelude.Maybe Prelude.Text,
    -- | Unset the existing @overrideLinkOutUri@ if it exists.
    removeOverrideLinkOutUri :: Prelude.Maybe Prelude.Bool,
    -- | The @revisionId@ of the content resource to update, taken from an
    -- earlier call to @GetContent@, @GetContentSummary@, @SearchContent@, or
    -- @ListContents@. If included, this argument acts as an optimistic lock to
    -- ensure content was not modified since it was last read. If it has been
    -- modified, this API throws a @PreconditionFailedException@.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The title of the content.
    title :: Prelude.Maybe Prelude.Text,
    -- | A pointer to the uploaded asset. This value is returned by
    -- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>.
    uploadId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the content. Can be either the ID or the ARN. URLs
    -- cannot contain the ARN.
    contentId :: Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'updateContent_metadata' - A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
--
-- 'overrideLinkOutUri', 'updateContent_overrideLinkOutUri' - The URI for the article. If the knowledge base has a templateUri,
-- setting this argument overrides it for this piece of content. To remove
-- an existing @overrideLinkOurUri@, exclude this argument and set
-- @removeOverrideLinkOutUri@ to true.
--
-- 'removeOverrideLinkOutUri', 'updateContent_removeOverrideLinkOutUri' - Unset the existing @overrideLinkOutUri@ if it exists.
--
-- 'revisionId', 'updateContent_revisionId' - The @revisionId@ of the content resource to update, taken from an
-- earlier call to @GetContent@, @GetContentSummary@, @SearchContent@, or
-- @ListContents@. If included, this argument acts as an optimistic lock to
-- ensure content was not modified since it was last read. If it has been
-- modified, this API throws a @PreconditionFailedException@.
--
-- 'title', 'updateContent_title' - The title of the content.
--
-- 'uploadId', 'updateContent_uploadId' - A pointer to the uploaded asset. This value is returned by
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>.
--
-- 'contentId', 'updateContent_contentId' - The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
--
-- 'knowledgeBaseId', 'updateContent_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN
newUpdateContent ::
  -- | 'contentId'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  UpdateContent
newUpdateContent pContentId_ pKnowledgeBaseId_ =
  UpdateContent'
    { metadata = Prelude.Nothing,
      overrideLinkOutUri = Prelude.Nothing,
      removeOverrideLinkOutUri = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      title = Prelude.Nothing,
      uploadId = Prelude.Nothing,
      contentId = pContentId_,
      knowledgeBaseId = pKnowledgeBaseId_
    }

-- | A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
updateContent_metadata :: Lens.Lens' UpdateContent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateContent_metadata = Lens.lens (\UpdateContent' {metadata} -> metadata) (\s@UpdateContent' {} a -> s {metadata = a} :: UpdateContent) Prelude.. Lens.mapping Lens.coerced

-- | The URI for the article. If the knowledge base has a templateUri,
-- setting this argument overrides it for this piece of content. To remove
-- an existing @overrideLinkOurUri@, exclude this argument and set
-- @removeOverrideLinkOutUri@ to true.
updateContent_overrideLinkOutUri :: Lens.Lens' UpdateContent (Prelude.Maybe Prelude.Text)
updateContent_overrideLinkOutUri = Lens.lens (\UpdateContent' {overrideLinkOutUri} -> overrideLinkOutUri) (\s@UpdateContent' {} a -> s {overrideLinkOutUri = a} :: UpdateContent)

-- | Unset the existing @overrideLinkOutUri@ if it exists.
updateContent_removeOverrideLinkOutUri :: Lens.Lens' UpdateContent (Prelude.Maybe Prelude.Bool)
updateContent_removeOverrideLinkOutUri = Lens.lens (\UpdateContent' {removeOverrideLinkOutUri} -> removeOverrideLinkOutUri) (\s@UpdateContent' {} a -> s {removeOverrideLinkOutUri = a} :: UpdateContent)

-- | The @revisionId@ of the content resource to update, taken from an
-- earlier call to @GetContent@, @GetContentSummary@, @SearchContent@, or
-- @ListContents@. If included, this argument acts as an optimistic lock to
-- ensure content was not modified since it was last read. If it has been
-- modified, this API throws a @PreconditionFailedException@.
updateContent_revisionId :: Lens.Lens' UpdateContent (Prelude.Maybe Prelude.Text)
updateContent_revisionId = Lens.lens (\UpdateContent' {revisionId} -> revisionId) (\s@UpdateContent' {} a -> s {revisionId = a} :: UpdateContent)

-- | The title of the content.
updateContent_title :: Lens.Lens' UpdateContent (Prelude.Maybe Prelude.Text)
updateContent_title = Lens.lens (\UpdateContent' {title} -> title) (\s@UpdateContent' {} a -> s {title = a} :: UpdateContent)

-- | A pointer to the uploaded asset. This value is returned by
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>.
updateContent_uploadId :: Lens.Lens' UpdateContent (Prelude.Maybe Prelude.Text)
updateContent_uploadId = Lens.lens (\UpdateContent' {uploadId} -> uploadId) (\s@UpdateContent' {} a -> s {uploadId = a} :: UpdateContent)

-- | The identifier of the content. Can be either the ID or the ARN. URLs
-- cannot contain the ARN.
updateContent_contentId :: Lens.Lens' UpdateContent Prelude.Text
updateContent_contentId = Lens.lens (\UpdateContent' {contentId} -> contentId) (\s@UpdateContent' {} a -> s {contentId = a} :: UpdateContent)

-- | The identifier of the knowledge base. Can be either the ID or the ARN
updateContent_knowledgeBaseId :: Lens.Lens' UpdateContent Prelude.Text
updateContent_knowledgeBaseId = Lens.lens (\UpdateContent' {knowledgeBaseId} -> knowledgeBaseId) (\s@UpdateContent' {} a -> s {knowledgeBaseId = a} :: UpdateContent)

instance Core.AWSRequest UpdateContent where
  type
    AWSResponse UpdateContent =
      UpdateContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContentResponse'
            Prelude.<$> (x Data..?> "content")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContent where
  hashWithSalt _salt UpdateContent' {..} =
    _salt `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` overrideLinkOutUri
      `Prelude.hashWithSalt` removeOverrideLinkOutUri
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` uploadId
      `Prelude.hashWithSalt` contentId
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData UpdateContent where
  rnf UpdateContent' {..} =
    Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf overrideLinkOutUri
      `Prelude.seq` Prelude.rnf removeOverrideLinkOutUri
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf contentId
      `Prelude.seq` Prelude.rnf knowledgeBaseId

instance Data.ToHeaders UpdateContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContent where
  toJSON UpdateContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metadata" Data..=) Prelude.<$> metadata,
            ("overrideLinkOutUri" Data..=)
              Prelude.<$> overrideLinkOutUri,
            ("removeOverrideLinkOutUri" Data..=)
              Prelude.<$> removeOverrideLinkOutUri,
            ("revisionId" Data..=) Prelude.<$> revisionId,
            ("title" Data..=) Prelude.<$> title,
            ("uploadId" Data..=) Prelude.<$> uploadId
          ]
      )

instance Data.ToPath UpdateContent where
  toPath UpdateContent' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/contents/",
        Data.toBS contentId
      ]

instance Data.ToQuery UpdateContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContentResponse' smart constructor.
data UpdateContentResponse = UpdateContentResponse'
  { -- | The content.
    content :: Prelude.Maybe ContentData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'updateContentResponse_content' - The content.
--
-- 'httpStatus', 'updateContentResponse_httpStatus' - The response's http status code.
newUpdateContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContentResponse
newUpdateContentResponse pHttpStatus_ =
  UpdateContentResponse'
    { content = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content.
updateContentResponse_content :: Lens.Lens' UpdateContentResponse (Prelude.Maybe ContentData)
updateContentResponse_content = Lens.lens (\UpdateContentResponse' {content} -> content) (\s@UpdateContentResponse' {} a -> s {content = a} :: UpdateContentResponse)

-- | The response's http status code.
updateContentResponse_httpStatus :: Lens.Lens' UpdateContentResponse Prelude.Int
updateContentResponse_httpStatus = Lens.lens (\UpdateContentResponse' {httpStatus} -> httpStatus) (\s@UpdateContentResponse' {} a -> s {httpStatus = a} :: UpdateContentResponse)

instance Prelude.NFData UpdateContentResponse where
  rnf UpdateContentResponse' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf httpStatus
