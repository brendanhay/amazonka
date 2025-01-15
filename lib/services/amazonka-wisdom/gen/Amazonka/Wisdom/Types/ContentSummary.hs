{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Wisdom.Types.ContentSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.ContentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.ContentStatus

-- | Summary information about the content.
--
-- /See:/ 'newContentSummary' smart constructor.
data ContentSummary = ContentSummary'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the content.
    contentArn :: Prelude.Text,
    -- | The identifier of the content.
    contentId :: Prelude.Text,
    -- | The media type of the content.
    contentType :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the knowledge base.
    knowledgeBaseArn :: Prelude.Text,
    -- | The identifier of the knowledge base.
    knowledgeBaseId :: Prelude.Text,
    -- | A key\/value map to store attributes without affecting tagging or
    -- recommendations. For example, when synchronizing data between an
    -- external system and Wisdom, you can store an external version identifier
    -- as metadata to utilize for determining drift.
    metadata :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The name of the content.
    name :: Prelude.Text,
    -- | The identifier of the revision of the content.
    revisionId :: Prelude.Text,
    -- | The status of the content.
    status :: ContentStatus,
    -- | The title of the content.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'contentSummary_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'contentArn', 'contentSummary_contentArn' - The Amazon Resource Name (ARN) of the content.
--
-- 'contentId', 'contentSummary_contentId' - The identifier of the content.
--
-- 'contentType', 'contentSummary_contentType' - The media type of the content.
--
-- 'knowledgeBaseArn', 'contentSummary_knowledgeBaseArn' - The Amazon Resource Name (ARN) of the knowledge base.
--
-- 'knowledgeBaseId', 'contentSummary_knowledgeBaseId' - The identifier of the knowledge base.
--
-- 'metadata', 'contentSummary_metadata' - A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
--
-- 'name', 'contentSummary_name' - The name of the content.
--
-- 'revisionId', 'contentSummary_revisionId' - The identifier of the revision of the content.
--
-- 'status', 'contentSummary_status' - The status of the content.
--
-- 'title', 'contentSummary_title' - The title of the content.
newContentSummary ::
  -- | 'contentArn'
  Prelude.Text ->
  -- | 'contentId'
  Prelude.Text ->
  -- | 'contentType'
  Prelude.Text ->
  -- | 'knowledgeBaseArn'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'status'
  ContentStatus ->
  -- | 'title'
  Prelude.Text ->
  ContentSummary
newContentSummary
  pContentArn_
  pContentId_
  pContentType_
  pKnowledgeBaseArn_
  pKnowledgeBaseId_
  pName_
  pRevisionId_
  pStatus_
  pTitle_ =
    ContentSummary'
      { tags = Prelude.Nothing,
        contentArn = pContentArn_,
        contentId = pContentId_,
        contentType = pContentType_,
        knowledgeBaseArn = pKnowledgeBaseArn_,
        knowledgeBaseId = pKnowledgeBaseId_,
        metadata = Prelude.mempty,
        name = pName_,
        revisionId = pRevisionId_,
        status = pStatus_,
        title = pTitle_
      }

-- | The tags used to organize, track, or control access for this resource.
contentSummary_tags :: Lens.Lens' ContentSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contentSummary_tags = Lens.lens (\ContentSummary' {tags} -> tags) (\s@ContentSummary' {} a -> s {tags = a} :: ContentSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the content.
contentSummary_contentArn :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_contentArn = Lens.lens (\ContentSummary' {contentArn} -> contentArn) (\s@ContentSummary' {} a -> s {contentArn = a} :: ContentSummary)

-- | The identifier of the content.
contentSummary_contentId :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_contentId = Lens.lens (\ContentSummary' {contentId} -> contentId) (\s@ContentSummary' {} a -> s {contentId = a} :: ContentSummary)

-- | The media type of the content.
contentSummary_contentType :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_contentType = Lens.lens (\ContentSummary' {contentType} -> contentType) (\s@ContentSummary' {} a -> s {contentType = a} :: ContentSummary)

-- | The Amazon Resource Name (ARN) of the knowledge base.
contentSummary_knowledgeBaseArn :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_knowledgeBaseArn = Lens.lens (\ContentSummary' {knowledgeBaseArn} -> knowledgeBaseArn) (\s@ContentSummary' {} a -> s {knowledgeBaseArn = a} :: ContentSummary)

-- | The identifier of the knowledge base.
contentSummary_knowledgeBaseId :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_knowledgeBaseId = Lens.lens (\ContentSummary' {knowledgeBaseId} -> knowledgeBaseId) (\s@ContentSummary' {} a -> s {knowledgeBaseId = a} :: ContentSummary)

-- | A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
contentSummary_metadata :: Lens.Lens' ContentSummary (Prelude.HashMap Prelude.Text Prelude.Text)
contentSummary_metadata = Lens.lens (\ContentSummary' {metadata} -> metadata) (\s@ContentSummary' {} a -> s {metadata = a} :: ContentSummary) Prelude.. Lens.coerced

-- | The name of the content.
contentSummary_name :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_name = Lens.lens (\ContentSummary' {name} -> name) (\s@ContentSummary' {} a -> s {name = a} :: ContentSummary)

-- | The identifier of the revision of the content.
contentSummary_revisionId :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_revisionId = Lens.lens (\ContentSummary' {revisionId} -> revisionId) (\s@ContentSummary' {} a -> s {revisionId = a} :: ContentSummary)

-- | The status of the content.
contentSummary_status :: Lens.Lens' ContentSummary ContentStatus
contentSummary_status = Lens.lens (\ContentSummary' {status} -> status) (\s@ContentSummary' {} a -> s {status = a} :: ContentSummary)

-- | The title of the content.
contentSummary_title :: Lens.Lens' ContentSummary Prelude.Text
contentSummary_title = Lens.lens (\ContentSummary' {title} -> title) (\s@ContentSummary' {} a -> s {title = a} :: ContentSummary)

instance Data.FromJSON ContentSummary where
  parseJSON =
    Data.withObject
      "ContentSummary"
      ( \x ->
          ContentSummary'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "contentArn")
            Prelude.<*> (x Data..: "contentId")
            Prelude.<*> (x Data..: "contentType")
            Prelude.<*> (x Data..: "knowledgeBaseArn")
            Prelude.<*> (x Data..: "knowledgeBaseId")
            Prelude.<*> (x Data..:? "metadata" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "revisionId")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "title")
      )

instance Prelude.Hashable ContentSummary where
  hashWithSalt _salt ContentSummary' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` contentArn
      `Prelude.hashWithSalt` contentId
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` knowledgeBaseArn
      `Prelude.hashWithSalt` knowledgeBaseId
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title

instance Prelude.NFData ContentSummary where
  rnf ContentSummary' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf contentArn `Prelude.seq`
        Prelude.rnf contentId `Prelude.seq`
          Prelude.rnf contentType `Prelude.seq`
            Prelude.rnf knowledgeBaseArn `Prelude.seq`
              Prelude.rnf knowledgeBaseId `Prelude.seq`
                Prelude.rnf metadata `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf revisionId `Prelude.seq`
                      Prelude.rnf status `Prelude.seq`
                        Prelude.rnf title
