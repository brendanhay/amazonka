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
-- Module      : Network.AWS.Textract.Types.S3Object
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Textract.Types.S3Object where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The S3 bucket name and file name that identifies the document.
--
-- The AWS Region for the S3 bucket that contains the document must match
-- the Region that you use for Amazon Textract operations.
--
-- For Amazon Textract to process a file in an S3 bucket, the user must
-- have permission to access the S3 bucket and file.
--
-- /See:/ 'newS3Object' smart constructor.
data S3Object = S3Object'
  { -- | If the bucket has versioning enabled, you can specify the object
    -- version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The file name of the input document. Synchronous operations can use
    -- image files that are in JPEG or PNG format. Asynchronous operations also
    -- support PDF format files.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket. Note that the # character is not valid in the
    -- file name.
    bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Object' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 's3Object_version' - If the bucket has versioning enabled, you can specify the object
-- version.
--
-- 'name', 's3Object_name' - The file name of the input document. Synchronous operations can use
-- image files that are in JPEG or PNG format. Asynchronous operations also
-- support PDF format files.
--
-- 'bucket', 's3Object_bucket' - The name of the S3 bucket. Note that the # character is not valid in the
-- file name.
newS3Object ::
  S3Object
newS3Object =
  S3Object'
    { version = Prelude.Nothing,
      name = Prelude.Nothing,
      bucket = Prelude.Nothing
    }

-- | If the bucket has versioning enabled, you can specify the object
-- version.
s3Object_version :: Lens.Lens' S3Object (Prelude.Maybe Prelude.Text)
s3Object_version = Lens.lens (\S3Object' {version} -> version) (\s@S3Object' {} a -> s {version = a} :: S3Object)

-- | The file name of the input document. Synchronous operations can use
-- image files that are in JPEG or PNG format. Asynchronous operations also
-- support PDF format files.
s3Object_name :: Lens.Lens' S3Object (Prelude.Maybe Prelude.Text)
s3Object_name = Lens.lens (\S3Object' {name} -> name) (\s@S3Object' {} a -> s {name = a} :: S3Object)

-- | The name of the S3 bucket. Note that the # character is not valid in the
-- file name.
s3Object_bucket :: Lens.Lens' S3Object (Prelude.Maybe Prelude.Text)
s3Object_bucket = Lens.lens (\S3Object' {bucket} -> bucket) (\s@S3Object' {} a -> s {bucket = a} :: S3Object)

instance Prelude.Hashable S3Object

instance Prelude.NFData S3Object

instance Core.ToJSON S3Object where
  toJSON S3Object' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Version" Core..=) Prelude.<$> version,
            ("Name" Core..=) Prelude.<$> name,
            ("Bucket" Core..=) Prelude.<$> bucket
          ]
      )
