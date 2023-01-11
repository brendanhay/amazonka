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
-- Module      : Amazonka.Wisdom.CreateContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates Wisdom content. Before to calling this API, use
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>
-- to upload an asset.
module Amazonka.Wisdom.CreateContent
  ( -- * Creating a Request
    CreateContent (..),
    newCreateContent,

    -- * Request Lenses
    createContent_clientToken,
    createContent_metadata,
    createContent_overrideLinkOutUri,
    createContent_tags,
    createContent_title,
    createContent_knowledgeBaseId,
    createContent_name,
    createContent_uploadId,

    -- * Destructuring the Response
    CreateContentResponse (..),
    newCreateContentResponse,

    -- * Response Lenses
    createContentResponse_content,
    createContentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newCreateContent' smart constructor.
data CreateContent = CreateContent'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A key\/value map to store attributes without affecting tagging or
    -- recommendations. For example, when synchronizing data between an
    -- external system and Wisdom, you can store an external version identifier
    -- as metadata to utilize for determining drift.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URI you want to use for the article. If the knowledge base has a
    -- templateUri, setting this argument overrides it for this piece of
    -- content.
    overrideLinkOutUri :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The title of the content. If not set, the title is equal to the name.
    title :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text,
    -- | The name of the content. Each piece of content in a knowledge base must
    -- have a unique name. You can retrieve a piece of content using only its
    -- knowledge base and its name with the
    -- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_SearchContent.html SearchContent>
    -- API.
    name :: Prelude.Text,
    -- | A pointer to the uploaded asset. This value is returned by
    -- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createContent_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'metadata', 'createContent_metadata' - A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
--
-- 'overrideLinkOutUri', 'createContent_overrideLinkOutUri' - The URI you want to use for the article. If the knowledge base has a
-- templateUri, setting this argument overrides it for this piece of
-- content.
--
-- 'tags', 'createContent_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'title', 'createContent_title' - The title of the content. If not set, the title is equal to the name.
--
-- 'knowledgeBaseId', 'createContent_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
--
-- 'name', 'createContent_name' - The name of the content. Each piece of content in a knowledge base must
-- have a unique name. You can retrieve a piece of content using only its
-- knowledge base and its name with the
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_SearchContent.html SearchContent>
-- API.
--
-- 'uploadId', 'createContent_uploadId' - A pointer to the uploaded asset. This value is returned by
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>.
newCreateContent ::
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  CreateContent
newCreateContent pKnowledgeBaseId_ pName_ pUploadId_ =
  CreateContent'
    { clientToken = Prelude.Nothing,
      metadata = Prelude.Nothing,
      overrideLinkOutUri = Prelude.Nothing,
      tags = Prelude.Nothing,
      title = Prelude.Nothing,
      knowledgeBaseId = pKnowledgeBaseId_,
      name = pName_,
      uploadId = pUploadId_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createContent_clientToken :: Lens.Lens' CreateContent (Prelude.Maybe Prelude.Text)
createContent_clientToken = Lens.lens (\CreateContent' {clientToken} -> clientToken) (\s@CreateContent' {} a -> s {clientToken = a} :: CreateContent)

-- | A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
createContent_metadata :: Lens.Lens' CreateContent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createContent_metadata = Lens.lens (\CreateContent' {metadata} -> metadata) (\s@CreateContent' {} a -> s {metadata = a} :: CreateContent) Prelude.. Lens.mapping Lens.coerced

-- | The URI you want to use for the article. If the knowledge base has a
-- templateUri, setting this argument overrides it for this piece of
-- content.
createContent_overrideLinkOutUri :: Lens.Lens' CreateContent (Prelude.Maybe Prelude.Text)
createContent_overrideLinkOutUri = Lens.lens (\CreateContent' {overrideLinkOutUri} -> overrideLinkOutUri) (\s@CreateContent' {} a -> s {overrideLinkOutUri = a} :: CreateContent)

-- | The tags used to organize, track, or control access for this resource.
createContent_tags :: Lens.Lens' CreateContent (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createContent_tags = Lens.lens (\CreateContent' {tags} -> tags) (\s@CreateContent' {} a -> s {tags = a} :: CreateContent) Prelude.. Lens.mapping Lens.coerced

-- | The title of the content. If not set, the title is equal to the name.
createContent_title :: Lens.Lens' CreateContent (Prelude.Maybe Prelude.Text)
createContent_title = Lens.lens (\CreateContent' {title} -> title) (\s@CreateContent' {} a -> s {title = a} :: CreateContent)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
createContent_knowledgeBaseId :: Lens.Lens' CreateContent Prelude.Text
createContent_knowledgeBaseId = Lens.lens (\CreateContent' {knowledgeBaseId} -> knowledgeBaseId) (\s@CreateContent' {} a -> s {knowledgeBaseId = a} :: CreateContent)

-- | The name of the content. Each piece of content in a knowledge base must
-- have a unique name. You can retrieve a piece of content using only its
-- knowledge base and its name with the
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_SearchContent.html SearchContent>
-- API.
createContent_name :: Lens.Lens' CreateContent Prelude.Text
createContent_name = Lens.lens (\CreateContent' {name} -> name) (\s@CreateContent' {} a -> s {name = a} :: CreateContent)

-- | A pointer to the uploaded asset. This value is returned by
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_StartContentUpload.html StartContentUpload>.
createContent_uploadId :: Lens.Lens' CreateContent Prelude.Text
createContent_uploadId = Lens.lens (\CreateContent' {uploadId} -> uploadId) (\s@CreateContent' {} a -> s {uploadId = a} :: CreateContent)

instance Core.AWSRequest CreateContent where
  type
    AWSResponse CreateContent =
      CreateContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContentResponse'
            Prelude.<$> (x Data..?> "content")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContent where
  hashWithSalt _salt CreateContent' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` overrideLinkOutUri
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` knowledgeBaseId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData CreateContent where
  rnf CreateContent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf overrideLinkOutUri
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf knowledgeBaseId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf uploadId

instance Data.ToHeaders CreateContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContent where
  toJSON CreateContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("metadata" Data..=) Prelude.<$> metadata,
            ("overrideLinkOutUri" Data..=)
              Prelude.<$> overrideLinkOutUri,
            ("tags" Data..=) Prelude.<$> tags,
            ("title" Data..=) Prelude.<$> title,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("uploadId" Data..= uploadId)
          ]
      )

instance Data.ToPath CreateContent where
  toPath CreateContent' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/contents"
      ]

instance Data.ToQuery CreateContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContentResponse' smart constructor.
data CreateContentResponse = CreateContentResponse'
  { -- | The content.
    content :: Prelude.Maybe ContentData,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'createContentResponse_content' - The content.
--
-- 'httpStatus', 'createContentResponse_httpStatus' - The response's http status code.
newCreateContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContentResponse
newCreateContentResponse pHttpStatus_ =
  CreateContentResponse'
    { content = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The content.
createContentResponse_content :: Lens.Lens' CreateContentResponse (Prelude.Maybe ContentData)
createContentResponse_content = Lens.lens (\CreateContentResponse' {content} -> content) (\s@CreateContentResponse' {} a -> s {content = a} :: CreateContentResponse)

-- | The response's http status code.
createContentResponse_httpStatus :: Lens.Lens' CreateContentResponse Prelude.Int
createContentResponse_httpStatus = Lens.lens (\CreateContentResponse' {httpStatus} -> httpStatus) (\s@CreateContentResponse' {} a -> s {httpStatus = a} :: CreateContentResponse)

instance Prelude.NFData CreateContentResponse where
  rnf CreateContentResponse' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf httpStatus
