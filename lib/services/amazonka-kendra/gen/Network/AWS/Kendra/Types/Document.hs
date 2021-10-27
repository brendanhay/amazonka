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
-- Module      : Network.AWS.Kendra.Types.Document
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Document where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ContentType
import Network.AWS.Kendra.Types.DocumentAttribute
import Network.AWS.Kendra.Types.HierarchicalPrincipal
import Network.AWS.Kendra.Types.Principal
import Network.AWS.Kendra.Types.S3Path
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A document in an index.
--
-- /See:/ 'newDocument' smart constructor.
data Document = Document'
  { -- | The contents of the document.
    --
    -- Documents passed to the @Blob@ parameter must be base64 encoded. Your
    -- code might not need to encode the document file bytes if you\'re using
    -- an Amazon Web Services SDK to call Amazon Kendra operations. If you are
    -- calling the Amazon Kendra endpoint directly using REST, you must base64
    -- encode the contents before sending.
    blob :: Prelude.Maybe Core.Base64,
    -- | Information on user and group access rights, which is used for user
    -- context filtering.
    accessControlList :: Prelude.Maybe [Principal],
    -- | Custom attributes to apply to the document. Use the custom attributes to
    -- provide additional information for searching, to provide facets for
    -- refining searches, and to provide additional information in the query
    -- response.
    attributes :: Prelude.Maybe [DocumentAttribute],
    s3Path :: Prelude.Maybe S3Path,
    -- | The title of the document.
    title :: Prelude.Maybe Prelude.Text,
    -- | The list of
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
    -- lists that define the hierarchy for which documents users should have
    -- access to.
    hierarchicalAccessControlList :: Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal),
    -- | The file type of the document in the @Blob@ field.
    contentType :: Prelude.Maybe ContentType,
    -- | A unique identifier of the document in the index.
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
-- 'blob', 'document_blob' - The contents of the document.
--
-- Documents passed to the @Blob@ parameter must be base64 encoded. Your
-- code might not need to encode the document file bytes if you\'re using
-- an Amazon Web Services SDK to call Amazon Kendra operations. If you are
-- calling the Amazon Kendra endpoint directly using REST, you must base64
-- encode the contents before sending.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'accessControlList', 'document_accessControlList' - Information on user and group access rights, which is used for user
-- context filtering.
--
-- 'attributes', 'document_attributes' - Custom attributes to apply to the document. Use the custom attributes to
-- provide additional information for searching, to provide facets for
-- refining searches, and to provide additional information in the query
-- response.
--
-- 's3Path', 'document_s3Path' - Undocumented member.
--
-- 'title', 'document_title' - The title of the document.
--
-- 'hierarchicalAccessControlList', 'document_hierarchicalAccessControlList' - The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
--
-- 'contentType', 'document_contentType' - The file type of the document in the @Blob@ field.
--
-- 'id', 'document_id' - A unique identifier of the document in the index.
newDocument ::
  -- | 'id'
  Prelude.Text ->
  Document
newDocument pId_ =
  Document'
    { blob = Prelude.Nothing,
      accessControlList = Prelude.Nothing,
      attributes = Prelude.Nothing,
      s3Path = Prelude.Nothing,
      title = Prelude.Nothing,
      hierarchicalAccessControlList = Prelude.Nothing,
      contentType = Prelude.Nothing,
      id = pId_
    }

-- | The contents of the document.
--
-- Documents passed to the @Blob@ parameter must be base64 encoded. Your
-- code might not need to encode the document file bytes if you\'re using
-- an Amazon Web Services SDK to call Amazon Kendra operations. If you are
-- calling the Amazon Kendra endpoint directly using REST, you must base64
-- encode the contents before sending.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
document_blob :: Lens.Lens' Document (Prelude.Maybe Prelude.ByteString)
document_blob = Lens.lens (\Document' {blob} -> blob) (\s@Document' {} a -> s {blob = a} :: Document) Prelude.. Lens.mapping Core._Base64

-- | Information on user and group access rights, which is used for user
-- context filtering.
document_accessControlList :: Lens.Lens' Document (Prelude.Maybe [Principal])
document_accessControlList = Lens.lens (\Document' {accessControlList} -> accessControlList) (\s@Document' {} a -> s {accessControlList = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | Custom attributes to apply to the document. Use the custom attributes to
-- provide additional information for searching, to provide facets for
-- refining searches, and to provide additional information in the query
-- response.
document_attributes :: Lens.Lens' Document (Prelude.Maybe [DocumentAttribute])
document_attributes = Lens.lens (\Document' {attributes} -> attributes) (\s@Document' {} a -> s {attributes = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
document_s3Path :: Lens.Lens' Document (Prelude.Maybe S3Path)
document_s3Path = Lens.lens (\Document' {s3Path} -> s3Path) (\s@Document' {} a -> s {s3Path = a} :: Document)

-- | The title of the document.
document_title :: Lens.Lens' Document (Prelude.Maybe Prelude.Text)
document_title = Lens.lens (\Document' {title} -> title) (\s@Document' {} a -> s {title = a} :: Document)

-- | The list of
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Principal.html principal>
-- lists that define the hierarchy for which documents users should have
-- access to.
document_hierarchicalAccessControlList :: Lens.Lens' Document (Prelude.Maybe (Prelude.NonEmpty HierarchicalPrincipal))
document_hierarchicalAccessControlList = Lens.lens (\Document' {hierarchicalAccessControlList} -> hierarchicalAccessControlList) (\s@Document' {} a -> s {hierarchicalAccessControlList = a} :: Document) Prelude.. Lens.mapping Lens.coerced

-- | The file type of the document in the @Blob@ field.
document_contentType :: Lens.Lens' Document (Prelude.Maybe ContentType)
document_contentType = Lens.lens (\Document' {contentType} -> contentType) (\s@Document' {} a -> s {contentType = a} :: Document)

-- | A unique identifier of the document in the index.
document_id :: Lens.Lens' Document Prelude.Text
document_id = Lens.lens (\Document' {id} -> id) (\s@Document' {} a -> s {id = a} :: Document)

instance Prelude.Hashable Document

instance Prelude.NFData Document

instance Core.ToJSON Document where
  toJSON Document' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Blob" Core..=) Prelude.<$> blob,
            ("AccessControlList" Core..=)
              Prelude.<$> accessControlList,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("S3Path" Core..=) Prelude.<$> s3Path,
            ("Title" Core..=) Prelude.<$> title,
            ("HierarchicalAccessControlList" Core..=)
              Prelude.<$> hierarchicalAccessControlList,
            ("ContentType" Core..=) Prelude.<$> contentType,
            Prelude.Just ("Id" Core..= id)
          ]
      )
