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
-- Module      : Amazonka.Textract.Types.Document
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Document where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.S3Object

-- | The input document, either as bytes or as an S3 object.
--
-- You pass image bytes to an Amazon Textract API operation by using the
-- @Bytes@ property. For example, you would use the @Bytes@ property to
-- pass a document loaded from a local file system. Image bytes passed by
-- using the @Bytes@ property must be base64 encoded. Your code might not
-- need to encode document file bytes if you\'re using an AWS SDK to call
-- Amazon Textract API operations.
--
-- You pass images stored in an S3 bucket to an Amazon Textract API
-- operation by using the @S3Object@ property. Documents stored in an S3
-- bucket don\'t need to be base64 encoded.
--
-- The AWS Region for the S3 bucket that contains the S3 object must match
-- the AWS Region that you use for Amazon Textract operations.
--
-- If you use the AWS CLI to call Amazon Textract operations, passing image
-- bytes using the Bytes property isn\'t supported. You must first upload
-- the document to an Amazon S3 bucket, and then call the operation using
-- the S3Object property.
--
-- For Amazon Textract to process an S3 object, the user must have
-- permission to access the S3 object.
--
-- /See:/ 'newDocument' smart constructor.
data Document = Document'
  { -- | A blob of base64-encoded document bytes. The maximum size of a document
    -- that\'s provided in a blob of bytes is 5 MB. The document bytes must be
    -- in PNG or JPEG format.
    --
    -- If you\'re using an AWS SDK to call Amazon Textract, you might not need
    -- to base64-encode image bytes passed using the @Bytes@ field.
    bytes :: Prelude.Maybe Data.Base64,
    -- | Identifies an S3 object as the document source. The maximum size of a
    -- document that\'s stored in an S3 bucket is 5 MB.
    s3Object :: Prelude.Maybe S3Object
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
-- 'bytes', 'document_bytes' - A blob of base64-encoded document bytes. The maximum size of a document
-- that\'s provided in a blob of bytes is 5 MB. The document bytes must be
-- in PNG or JPEG format.
--
-- If you\'re using an AWS SDK to call Amazon Textract, you might not need
-- to base64-encode image bytes passed using the @Bytes@ field.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3Object', 'document_s3Object' - Identifies an S3 object as the document source. The maximum size of a
-- document that\'s stored in an S3 bucket is 5 MB.
newDocument ::
  Document
newDocument =
  Document'
    { bytes = Prelude.Nothing,
      s3Object = Prelude.Nothing
    }

-- | A blob of base64-encoded document bytes. The maximum size of a document
-- that\'s provided in a blob of bytes is 5 MB. The document bytes must be
-- in PNG or JPEG format.
--
-- If you\'re using an AWS SDK to call Amazon Textract, you might not need
-- to base64-encode image bytes passed using the @Bytes@ field.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
document_bytes :: Lens.Lens' Document (Prelude.Maybe Prelude.ByteString)
document_bytes = Lens.lens (\Document' {bytes} -> bytes) (\s@Document' {} a -> s {bytes = a} :: Document) Prelude.. Lens.mapping Data._Base64

-- | Identifies an S3 object as the document source. The maximum size of a
-- document that\'s stored in an S3 bucket is 5 MB.
document_s3Object :: Lens.Lens' Document (Prelude.Maybe S3Object)
document_s3Object = Lens.lens (\Document' {s3Object} -> s3Object) (\s@Document' {} a -> s {s3Object = a} :: Document)

instance Prelude.Hashable Document where
  hashWithSalt _salt Document' {..} =
    _salt `Prelude.hashWithSalt` bytes
      `Prelude.hashWithSalt` s3Object

instance Prelude.NFData Document where
  rnf Document' {..} =
    Prelude.rnf bytes
      `Prelude.seq` Prelude.rnf s3Object

instance Data.ToJSON Document where
  toJSON Document' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bytes" Data..=) Prelude.<$> bytes,
            ("S3Object" Data..=) Prelude.<$> s3Object
          ]
      )
