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
-- Module      : Amazonka.Kendra.Types.DocumentsMetadataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentsMetadataConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Document metadata files that contain information such as the document
-- access control information, source URI, document author, and custom
-- attributes. Each metadata file contains metadata about a single
-- document.
--
-- /See:/ 'newDocumentsMetadataConfiguration' smart constructor.
data DocumentsMetadataConfiguration = DocumentsMetadataConfiguration'
  { -- | A prefix used to filter metadata configuration files in the Amazon Web
    -- Services S3 bucket. The S3 bucket might contain multiple metadata files.
    -- Use @S3Prefix@ to include only the desired metadata files.
    s3Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentsMetadataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Prefix', 'documentsMetadataConfiguration_s3Prefix' - A prefix used to filter metadata configuration files in the Amazon Web
-- Services S3 bucket. The S3 bucket might contain multiple metadata files.
-- Use @S3Prefix@ to include only the desired metadata files.
newDocumentsMetadataConfiguration ::
  DocumentsMetadataConfiguration
newDocumentsMetadataConfiguration =
  DocumentsMetadataConfiguration'
    { s3Prefix =
        Prelude.Nothing
    }

-- | A prefix used to filter metadata configuration files in the Amazon Web
-- Services S3 bucket. The S3 bucket might contain multiple metadata files.
-- Use @S3Prefix@ to include only the desired metadata files.
documentsMetadataConfiguration_s3Prefix :: Lens.Lens' DocumentsMetadataConfiguration (Prelude.Maybe Prelude.Text)
documentsMetadataConfiguration_s3Prefix = Lens.lens (\DocumentsMetadataConfiguration' {s3Prefix} -> s3Prefix) (\s@DocumentsMetadataConfiguration' {} a -> s {s3Prefix = a} :: DocumentsMetadataConfiguration)

instance Data.FromJSON DocumentsMetadataConfiguration where
  parseJSON =
    Data.withObject
      "DocumentsMetadataConfiguration"
      ( \x ->
          DocumentsMetadataConfiguration'
            Prelude.<$> (x Data..:? "S3Prefix")
      )

instance
  Prelude.Hashable
    DocumentsMetadataConfiguration
  where
  hashWithSalt
    _salt
    DocumentsMetadataConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3Prefix

instance
  Prelude.NFData
    DocumentsMetadataConfiguration
  where
  rnf DocumentsMetadataConfiguration' {..} =
    Prelude.rnf s3Prefix

instance Data.ToJSON DocumentsMetadataConfiguration where
  toJSON DocumentsMetadataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Prefix" Data..=) Prelude.<$> s3Prefix]
      )
