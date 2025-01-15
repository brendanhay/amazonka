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
-- Module      : Amazonka.Wisdom.Types.ContentData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.ContentData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.ContentStatus

-- | Information about the content.
--
-- /See:/ 'newContentData' smart constructor.
data ContentData = ContentData'
  { -- | The URI of the content.
    linkOutUri :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
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
    -- | The identifier of the content revision.
    revisionId :: Prelude.Text,
    -- | The status of the content.
    status :: ContentStatus,
    -- | The title of the content.
    title :: Prelude.Text,
    -- | The URL of the content.
    url :: Data.Sensitive Prelude.Text,
    -- | The expiration time of the URL as an epoch timestamp.
    urlExpiry :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linkOutUri', 'contentData_linkOutUri' - The URI of the content.
--
-- 'tags', 'contentData_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'contentArn', 'contentData_contentArn' - The Amazon Resource Name (ARN) of the content.
--
-- 'contentId', 'contentData_contentId' - The identifier of the content.
--
-- 'contentType', 'contentData_contentType' - The media type of the content.
--
-- 'knowledgeBaseArn', 'contentData_knowledgeBaseArn' - The Amazon Resource Name (ARN) of the knowledge base.
--
-- 'knowledgeBaseId', 'contentData_knowledgeBaseId' - The identifier of the knowledge base.
--
-- 'metadata', 'contentData_metadata' - A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
--
-- 'name', 'contentData_name' - The name of the content.
--
-- 'revisionId', 'contentData_revisionId' - The identifier of the content revision.
--
-- 'status', 'contentData_status' - The status of the content.
--
-- 'title', 'contentData_title' - The title of the content.
--
-- 'url', 'contentData_url' - The URL of the content.
--
-- 'urlExpiry', 'contentData_urlExpiry' - The expiration time of the URL as an epoch timestamp.
newContentData ::
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
  -- | 'url'
  Prelude.Text ->
  -- | 'urlExpiry'
  Prelude.UTCTime ->
  ContentData
newContentData
  pContentArn_
  pContentId_
  pContentType_
  pKnowledgeBaseArn_
  pKnowledgeBaseId_
  pName_
  pRevisionId_
  pStatus_
  pTitle_
  pUrl_
  pUrlExpiry_ =
    ContentData'
      { linkOutUri = Prelude.Nothing,
        tags = Prelude.Nothing,
        contentArn = pContentArn_,
        contentId = pContentId_,
        contentType = pContentType_,
        knowledgeBaseArn = pKnowledgeBaseArn_,
        knowledgeBaseId = pKnowledgeBaseId_,
        metadata = Prelude.mempty,
        name = pName_,
        revisionId = pRevisionId_,
        status = pStatus_,
        title = pTitle_,
        url = Data._Sensitive Lens.# pUrl_,
        urlExpiry = Data._Time Lens.# pUrlExpiry_
      }

-- | The URI of the content.
contentData_linkOutUri :: Lens.Lens' ContentData (Prelude.Maybe Prelude.Text)
contentData_linkOutUri = Lens.lens (\ContentData' {linkOutUri} -> linkOutUri) (\s@ContentData' {} a -> s {linkOutUri = a} :: ContentData)

-- | The tags used to organize, track, or control access for this resource.
contentData_tags :: Lens.Lens' ContentData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
contentData_tags = Lens.lens (\ContentData' {tags} -> tags) (\s@ContentData' {} a -> s {tags = a} :: ContentData) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the content.
contentData_contentArn :: Lens.Lens' ContentData Prelude.Text
contentData_contentArn = Lens.lens (\ContentData' {contentArn} -> contentArn) (\s@ContentData' {} a -> s {contentArn = a} :: ContentData)

-- | The identifier of the content.
contentData_contentId :: Lens.Lens' ContentData Prelude.Text
contentData_contentId = Lens.lens (\ContentData' {contentId} -> contentId) (\s@ContentData' {} a -> s {contentId = a} :: ContentData)

-- | The media type of the content.
contentData_contentType :: Lens.Lens' ContentData Prelude.Text
contentData_contentType = Lens.lens (\ContentData' {contentType} -> contentType) (\s@ContentData' {} a -> s {contentType = a} :: ContentData)

-- | The Amazon Resource Name (ARN) of the knowledge base.
contentData_knowledgeBaseArn :: Lens.Lens' ContentData Prelude.Text
contentData_knowledgeBaseArn = Lens.lens (\ContentData' {knowledgeBaseArn} -> knowledgeBaseArn) (\s@ContentData' {} a -> s {knowledgeBaseArn = a} :: ContentData)

-- | The identifier of the knowledge base.
contentData_knowledgeBaseId :: Lens.Lens' ContentData Prelude.Text
contentData_knowledgeBaseId = Lens.lens (\ContentData' {knowledgeBaseId} -> knowledgeBaseId) (\s@ContentData' {} a -> s {knowledgeBaseId = a} :: ContentData)

-- | A key\/value map to store attributes without affecting tagging or
-- recommendations. For example, when synchronizing data between an
-- external system and Wisdom, you can store an external version identifier
-- as metadata to utilize for determining drift.
contentData_metadata :: Lens.Lens' ContentData (Prelude.HashMap Prelude.Text Prelude.Text)
contentData_metadata = Lens.lens (\ContentData' {metadata} -> metadata) (\s@ContentData' {} a -> s {metadata = a} :: ContentData) Prelude.. Lens.coerced

-- | The name of the content.
contentData_name :: Lens.Lens' ContentData Prelude.Text
contentData_name = Lens.lens (\ContentData' {name} -> name) (\s@ContentData' {} a -> s {name = a} :: ContentData)

-- | The identifier of the content revision.
contentData_revisionId :: Lens.Lens' ContentData Prelude.Text
contentData_revisionId = Lens.lens (\ContentData' {revisionId} -> revisionId) (\s@ContentData' {} a -> s {revisionId = a} :: ContentData)

-- | The status of the content.
contentData_status :: Lens.Lens' ContentData ContentStatus
contentData_status = Lens.lens (\ContentData' {status} -> status) (\s@ContentData' {} a -> s {status = a} :: ContentData)

-- | The title of the content.
contentData_title :: Lens.Lens' ContentData Prelude.Text
contentData_title = Lens.lens (\ContentData' {title} -> title) (\s@ContentData' {} a -> s {title = a} :: ContentData)

-- | The URL of the content.
contentData_url :: Lens.Lens' ContentData Prelude.Text
contentData_url = Lens.lens (\ContentData' {url} -> url) (\s@ContentData' {} a -> s {url = a} :: ContentData) Prelude.. Data._Sensitive

-- | The expiration time of the URL as an epoch timestamp.
contentData_urlExpiry :: Lens.Lens' ContentData Prelude.UTCTime
contentData_urlExpiry = Lens.lens (\ContentData' {urlExpiry} -> urlExpiry) (\s@ContentData' {} a -> s {urlExpiry = a} :: ContentData) Prelude.. Data._Time

instance Data.FromJSON ContentData where
  parseJSON =
    Data.withObject
      "ContentData"
      ( \x ->
          ContentData'
            Prelude.<$> (x Data..:? "linkOutUri")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
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
            Prelude.<*> (x Data..: "url")
            Prelude.<*> (x Data..: "urlExpiry")
      )

instance Prelude.Hashable ContentData where
  hashWithSalt _salt ContentData' {..} =
    _salt
      `Prelude.hashWithSalt` linkOutUri
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
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` urlExpiry

instance Prelude.NFData ContentData where
  rnf ContentData' {..} =
    Prelude.rnf linkOutUri `Prelude.seq`
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
                          Prelude.rnf title `Prelude.seq`
                            Prelude.rnf url `Prelude.seq`
                              Prelude.rnf urlExpiry
