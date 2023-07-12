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
-- Module      : Amazonka.Kendra.Types.Document
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Document where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ContentType
import Amazonka.Kendra.Types.DocumentAttribute
import Amazonka.Kendra.Types.HierarchicalPrincipal
import Amazonka.Kendra.Types.Principal
import Amazonka.Kendra.Types.S3Path
import qualified Amazonka.Prelude as Prelude

-- | A document in an index.
--
-- /See:/ 'newDocument' smart constructor.
data Document = Document'
  { -- | The identifier of the access control configuration that you want to
    -- apply to the document.
    accessControlConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | Information on principals (users and\/or groups) and which documents
    -- they should have access to. This is useful for user context filtering,
    -- where search results are filtered based on the user or their group
    -- access to documents.
    accessControlList :: Prelude.Maybe [Principal],
    -- | Custom attributes to apply to the document. Use the custom attributes to
    -- provide additional information for searching, to provide facets for
    -- refining searches, and to provide additional information in the query
    -- response.
    --
    -- For example, \'DataSourceId\' and \'DataSourceSyncJobId\' are custom
    -- attributes that provide information on the synchronization of documents
    -- running on a data source. Note, \'DataSourceSyncJobId\' could be an
    -- optional custom attribute as Amazon Kendra will use the ID of a running
    -- sync job.
    attributes :: Prelude.Maybe [DocumentAttribute],
    -- | The contents of the document.
    --
    -- Documents passed to the @Blob@ parameter must be base64 encoded. Your
    -- code might not need to encode the document file bytes if you\'re using
    -- an Amazon Web Services SDK to call Amazon Kendra APIs. If you are
    -- calling the Amazon Kendra endpoint directly using REST, you must base64
    -- encode the contents before sending.
    blob :: Prelude.Maybe Data.Base64,
    -- | The file type of the document in the @Blob@ field.
    contentType :: Prelude.Maybe ContentType,
    -- | The list of
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
    -- lists that define the hierarchy for which documents users should have
    -- access to.
    hierarchicalAccessControlList :: Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal),
    s3Path :: Prelude.Maybe S3Path,
    -- | The title of the document.
    title :: Prelude.Maybe Prelude.Text,
    -- | A identifier of the document in the index.
    --
    -- Note, each document ID must be unique per index. You cannot create a
    -- data source to index your documents with their unique IDs and then use
    -- the @BatchPutDocument@ API to index the same documents, or vice versa.
    -- You can delete a data source and then use the @BatchPutDocument@ API to
    -- index the same documents, or vice versa.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Document' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlConfigurationId', 'document_accessControlConfigurationId' - The identifier of the access control configuration that you want to
-- apply to the document.
--
-- 'accessControlList', 'document_accessControlList' - Information on principals (users and\/or groups) and which documents
-- they should have access to. This is useful for user context filtering,
-- where search results are filtered based on the user or their group
-- access to documents.
--
-- 'attributes', 'document_attributes' - Custom attributes to apply to the document. Use the custom attributes to
-- provide additional information for searching, to provide facets for
-- refining searches, and to provide additional information in the query
-- response.
--
-- For example, \'DataSourceId\' and \'DataSourceSyncJobId\' are custom
-- attributes that provide information on the synchronization of documents
-- running on a data source. Note, \'DataSourceSyncJobId\' could be an
-- optional custom attribute as Amazon Kendra will use the ID of a running
-- sync job.
--
-- 'blob', 'document_blob' - The contents of the document.
--
-- Documents passed to the @Blob@ parameter must be base64 encoded. Your
-- code might not need to encode the document file bytes if you\'re using
-- an Amazon Web Services SDK to call Amazon Kendra APIs. If you are
-- calling the Amazon Kendra endpoint directly using REST, you must base64
-- encode the contents before sending.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'contentType', 'document_contentType' - The file type of the document in the @Blob@ field.
--
-- 'hierarchicalAccessControlList', 'document_hierarchicalAccessControlList' - The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
--
-- 's3Path', 'document_s3Path' - Undocumented member.
--
-- 'title', 'document_title' - The title of the document.
--
-- 'id', 'document_id' - A identifier of the document in the index.
--
-- Note, each document ID must be unique per index. You cannot create a
-- data source to index your documents with their unique IDs and then use
-- the @BatchPutDocument@ API to index the same documents, or vice versa.
-- You can delete a data source and then use the @BatchPutDocument@ API to
-- index the same documents, or vice versa.
newDocument ::
  -- | 'id'
  Prelude.Text ->
  Document
newDocument pId_ =
  Document'
    { accessControlConfigurationId =
        Prelude.Nothing,
      accessControlList = Prelude.Nothing,
      attributes = Prelude.Nothing,
      blob = Prelude.Nothing,
      contentType = Prelude.Nothing,
      hierarchicalAccessControlList = Prelude.Nothing,
      s3Path = Prelude.Nothing,
      title = Prelude.Nothing,
      id = pId_
    }

-- | The identifier of the access control configuration that you want to
-- apply to the document.
document_accessControlConfigurationId :: Lens.Lens' Document (Prelude.Maybe Prelude.Text)
document_accessControlConfigurationId = Lens.lens (\Document' {accessControlConfigurationId} -> accessControlConfigurationId) (\s@Document' {} a -> s {accessControlConfigurationId = a} :: Document)

-- | Information on principals (users and\/or groups) and which documents
-- they should have access to. This is useful for user context filtering,
-- where search results are filtered based on the user or their group
-- access to documents.
document_accessControlList :: Lens.Lens' Document (Prelude.Maybe [Principal])
document_accessControlList = Lens.lens (\Document' {accessControlList} -> accessControlList) (\s@Document' {} a -> s {accessControlList = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | Custom attributes to apply to the document. Use the custom attributes to
-- provide additional information for searching, to provide facets for
-- refining searches, and to provide additional information in the query
-- response.
--
-- For example, \'DataSourceId\' and \'DataSourceSyncJobId\' are custom
-- attributes that provide information on the synchronization of documents
-- running on a data source. Note, \'DataSourceSyncJobId\' could be an
-- optional custom attribute as Amazon Kendra will use the ID of a running
-- sync job.
document_attributes :: Lens.Lens' Document (Prelude.Maybe [DocumentAttribute])
document_attributes = Lens.lens (\Document' {attributes} -> attributes) (\s@Document' {} a -> s {attributes = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | The contents of the document.
--
-- Documents passed to the @Blob@ parameter must be base64 encoded. Your
-- code might not need to encode the document file bytes if you\'re using
-- an Amazon Web Services SDK to call Amazon Kendra APIs. If you are
-- calling the Amazon Kendra endpoint directly using REST, you must base64
-- encode the contents before sending.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
document_blob :: Lens.Lens' Document (Prelude.Maybe Prelude.ByteString)
document_blob = Lens.lens (\Document' {blob} -> blob) (\s@Document' {} a -> s {blob = a} :: Document) Prelude.. Lens.mapping Data._Base64

-- | The file type of the document in the @Blob@ field.
document_contentType :: Lens.Lens' Document (Prelude.Maybe ContentType)
document_contentType = Lens.lens (\Document' {contentType} -> contentType) (\s@Document' {} a -> s {contentType = a} :: Document)

-- | The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
document_hierarchicalAccessControlList :: Lens.Lens' Document (Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal))
document_hierarchicalAccessControlList = Lens.lens (\Document' {hierarchicalAccessControlList} -> hierarchicalAccessControlList) (\s@Document' {} a -> s {hierarchicalAccessControlList = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
document_s3Path :: Lens.Lens' Document (Prelude.Maybe S3Path)
document_s3Path = Lens.lens (\Document' {s3Path} -> s3Path) (\s@Document' {} a -> s {s3Path = a} :: Document)

-- | The title of the document.
document_title :: Lens.Lens' Document (Prelude.Maybe Prelude.Text)
document_title = Lens.lens (\Document' {title} -> title) (\s@Document' {} a -> s {title = a} :: Document)

-- | A identifier of the document in the index.
--
-- Note, each document ID must be unique per index. You cannot create a
-- data source to index your documents with their unique IDs and then use
-- the @BatchPutDocument@ API to index the same documents, or vice versa.
-- You can delete a data source and then use the @BatchPutDocument@ API to
-- index the same documents, or vice versa.
document_id :: Lens.Lens' Document Prelude.Text
document_id = Lens.lens (\Document' {id} -> id) (\s@Document' {} a -> s {id = a} :: Document)

instance Prelude.Hashable Document where
  hashWithSalt _salt Document' {..} =
    _salt
      `Prelude.hashWithSalt` accessControlConfigurationId
      `Prelude.hashWithSalt` accessControlList
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` blob
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` hierarchicalAccessControlList
      `Prelude.hashWithSalt` s3Path
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` id

instance Prelude.NFData Document where
  rnf Document' {..} =
    Prelude.rnf accessControlConfigurationId
      `Prelude.seq` Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf blob
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf hierarchicalAccessControlList
      `Prelude.seq` Prelude.rnf s3Path
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf id

instance Data.ToJSON Document where
  toJSON Document' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessControlConfigurationId" Data..=)
              Prelude.<$> accessControlConfigurationId,
            ("AccessControlList" Data..=)
              Prelude.<$> accessControlList,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("Blob" Data..=) Prelude.<$> blob,
            ("ContentType" Data..=) Prelude.<$> contentType,
            ("HierarchicalAccessControlList" Data..=)
              Prelude.<$> hierarchicalAccessControlList,
            ("S3Path" Data..=) Prelude.<$> s3Path,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("Id" Data..= id)
          ]
      )
