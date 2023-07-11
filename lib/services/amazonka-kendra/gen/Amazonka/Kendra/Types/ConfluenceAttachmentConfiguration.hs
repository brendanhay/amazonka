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
-- Module      : Amazonka.Kendra.Types.ConfluenceAttachmentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ConfluenceAttachmentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Configuration of attachment settings for the Confluence data source.
-- Attachment settings are optional, if you don\'t specify settings
-- attachments, Amazon Kendra won\'t index them.
--
-- /See:/ 'newConfluenceAttachmentConfiguration' smart constructor.
data ConfluenceAttachmentConfiguration = ConfluenceAttachmentConfiguration'
  { -- | Maps attributes or field names of Confluence attachments to Amazon
    -- Kendra index field names. To create custom fields, use the @UpdateIndex@
    -- API before you map to Confluence fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Confluence data source field names must exist in your Confluence
    -- custom metadata.
    --
    -- If you specify the @AttachentFieldMappings@ parameter, you must specify
    -- at least one field mapping.
    attachmentFieldMappings :: Prelude.Maybe (Prelude.NonEmpty ConfluenceAttachmentToIndexFieldMapping),
    -- | @TRUE@ to index attachments of pages and blogs in Confluence.
    crawlAttachments :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfluenceAttachmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentFieldMappings', 'confluenceAttachmentConfiguration_attachmentFieldMappings' - Maps attributes or field names of Confluence attachments to Amazon
-- Kendra index field names. To create custom fields, use the @UpdateIndex@
-- API before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- If you specify the @AttachentFieldMappings@ parameter, you must specify
-- at least one field mapping.
--
-- 'crawlAttachments', 'confluenceAttachmentConfiguration_crawlAttachments' - @TRUE@ to index attachments of pages and blogs in Confluence.
newConfluenceAttachmentConfiguration ::
  ConfluenceAttachmentConfiguration
newConfluenceAttachmentConfiguration =
  ConfluenceAttachmentConfiguration'
    { attachmentFieldMappings =
        Prelude.Nothing,
      crawlAttachments = Prelude.Nothing
    }

-- | Maps attributes or field names of Confluence attachments to Amazon
-- Kendra index field names. To create custom fields, use the @UpdateIndex@
-- API before you map to Confluence fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Confluence data source field names must exist in your Confluence
-- custom metadata.
--
-- If you specify the @AttachentFieldMappings@ parameter, you must specify
-- at least one field mapping.
confluenceAttachmentConfiguration_attachmentFieldMappings :: Lens.Lens' ConfluenceAttachmentConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluenceAttachmentToIndexFieldMapping))
confluenceAttachmentConfiguration_attachmentFieldMappings = Lens.lens (\ConfluenceAttachmentConfiguration' {attachmentFieldMappings} -> attachmentFieldMappings) (\s@ConfluenceAttachmentConfiguration' {} a -> s {attachmentFieldMappings = a} :: ConfluenceAttachmentConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to index attachments of pages and blogs in Confluence.
confluenceAttachmentConfiguration_crawlAttachments :: Lens.Lens' ConfluenceAttachmentConfiguration (Prelude.Maybe Prelude.Bool)
confluenceAttachmentConfiguration_crawlAttachments = Lens.lens (\ConfluenceAttachmentConfiguration' {crawlAttachments} -> crawlAttachments) (\s@ConfluenceAttachmentConfiguration' {} a -> s {crawlAttachments = a} :: ConfluenceAttachmentConfiguration)

instance
  Data.FromJSON
    ConfluenceAttachmentConfiguration
  where
  parseJSON =
    Data.withObject
      "ConfluenceAttachmentConfiguration"
      ( \x ->
          ConfluenceAttachmentConfiguration'
            Prelude.<$> (x Data..:? "AttachmentFieldMappings")
            Prelude.<*> (x Data..:? "CrawlAttachments")
      )

instance
  Prelude.Hashable
    ConfluenceAttachmentConfiguration
  where
  hashWithSalt
    _salt
    ConfluenceAttachmentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` attachmentFieldMappings
        `Prelude.hashWithSalt` crawlAttachments

instance
  Prelude.NFData
    ConfluenceAttachmentConfiguration
  where
  rnf ConfluenceAttachmentConfiguration' {..} =
    Prelude.rnf attachmentFieldMappings
      `Prelude.seq` Prelude.rnf crawlAttachments

instance
  Data.ToJSON
    ConfluenceAttachmentConfiguration
  where
  toJSON ConfluenceAttachmentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttachmentFieldMappings" Data..=)
              Prelude.<$> attachmentFieldMappings,
            ("CrawlAttachments" Data..=)
              Prelude.<$> crawlAttachments
          ]
      )
