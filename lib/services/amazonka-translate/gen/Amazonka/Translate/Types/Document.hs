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
-- Module      : Amazonka.Translate.Types.Document
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.Document where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The content and content type of a document.
--
-- /See:/ 'newDocument' smart constructor.
data Document = Document'
  { -- | The @Content@field type is Binary large object (blob). This object
    -- contains the document content converted into base64-encoded binary data.
    -- If you use one of the AWS SDKs, the SDK performs the Base64-encoding on
    -- this field before sending the request.
    content :: Data.Sensitive Data.Base64,
    -- | Describes the format of the document. You can specify one of the
    -- following:
    --
    -- -   text\/html - The input data consists of HTML content. Amazon
    --     Translate translates only the text in the HTML element.
    --
    -- -   text\/plain - The input data consists of unformatted text. Amazon
    --     Translate translates every character in the content.
    contentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Document' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'document_content' - The @Content@field type is Binary large object (blob). This object
-- contains the document content converted into base64-encoded binary data.
-- If you use one of the AWS SDKs, the SDK performs the Base64-encoding on
-- this field before sending the request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'contentType', 'document_contentType' - Describes the format of the document. You can specify one of the
-- following:
--
-- -   text\/html - The input data consists of HTML content. Amazon
--     Translate translates only the text in the HTML element.
--
-- -   text\/plain - The input data consists of unformatted text. Amazon
--     Translate translates every character in the content.
newDocument ::
  -- | 'content'
  Prelude.ByteString ->
  -- | 'contentType'
  Prelude.Text ->
  Document
newDocument pContent_ pContentType_ =
  Document'
    { content =
        Data._Sensitive
          Prelude.. Data._Base64
          Lens.# pContent_,
      contentType = pContentType_
    }

-- | The @Content@field type is Binary large object (blob). This object
-- contains the document content converted into base64-encoded binary data.
-- If you use one of the AWS SDKs, the SDK performs the Base64-encoding on
-- this field before sending the request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
document_content :: Lens.Lens' Document Prelude.ByteString
document_content = Lens.lens (\Document' {content} -> content) (\s@Document' {} a -> s {content = a} :: Document) Prelude.. Data._Sensitive Prelude.. Data._Base64

-- | Describes the format of the document. You can specify one of the
-- following:
--
-- -   text\/html - The input data consists of HTML content. Amazon
--     Translate translates only the text in the HTML element.
--
-- -   text\/plain - The input data consists of unformatted text. Amazon
--     Translate translates every character in the content.
document_contentType :: Lens.Lens' Document Prelude.Text
document_contentType = Lens.lens (\Document' {contentType} -> contentType) (\s@Document' {} a -> s {contentType = a} :: Document)

instance Prelude.Hashable Document where
  hashWithSalt _salt Document' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` contentType

instance Prelude.NFData Document where
  rnf Document' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf contentType

instance Data.ToJSON Document where
  toJSON Document' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Content" Data..= content),
            Prelude.Just ("ContentType" Data..= contentType)
          ]
      )
