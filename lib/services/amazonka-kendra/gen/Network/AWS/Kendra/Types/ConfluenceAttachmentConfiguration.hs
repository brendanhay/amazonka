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
-- Module      : Network.AWS.Kendra.Types.ConfluenceAttachmentConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ConfluenceAttachmentConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the attachment settings for the Confluence data source.
-- Attachment settings are optional, if you don\'t specify settings
-- attachments, Amazon Kendra won\'t index them.
--
-- /See:/ 'newConfluenceAttachmentConfiguration' smart constructor.
data ConfluenceAttachmentConfiguration = ConfluenceAttachmentConfiguration'
  { -- | Indicates whether Amazon Kendra indexes attachments to the pages and
    -- blogs in the Confluence data source.
    crawlAttachments :: Prelude.Maybe Prelude.Bool,
    -- | Defines how attachment metadata fields should be mapped to index fields.
    -- Before you can map a field, you must first create an index field with a
    -- matching type using the console or the @UpdateIndex@ operation.
    --
    -- If you specify the @AttachentFieldMappings@ parameter, you must specify
    -- at least one field mapping.
    attachmentFieldMappings :: Prelude.Maybe (Prelude.NonEmpty ConfluenceAttachmentToIndexFieldMapping)
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
-- 'crawlAttachments', 'confluenceAttachmentConfiguration_crawlAttachments' - Indicates whether Amazon Kendra indexes attachments to the pages and
-- blogs in the Confluence data source.
--
-- 'attachmentFieldMappings', 'confluenceAttachmentConfiguration_attachmentFieldMappings' - Defines how attachment metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @AttachentFieldMappings@ parameter, you must specify
-- at least one field mapping.
newConfluenceAttachmentConfiguration ::
  ConfluenceAttachmentConfiguration
newConfluenceAttachmentConfiguration =
  ConfluenceAttachmentConfiguration'
    { crawlAttachments =
        Prelude.Nothing,
      attachmentFieldMappings =
        Prelude.Nothing
    }

-- | Indicates whether Amazon Kendra indexes attachments to the pages and
-- blogs in the Confluence data source.
confluenceAttachmentConfiguration_crawlAttachments :: Lens.Lens' ConfluenceAttachmentConfiguration (Prelude.Maybe Prelude.Bool)
confluenceAttachmentConfiguration_crawlAttachments = Lens.lens (\ConfluenceAttachmentConfiguration' {crawlAttachments} -> crawlAttachments) (\s@ConfluenceAttachmentConfiguration' {} a -> s {crawlAttachments = a} :: ConfluenceAttachmentConfiguration)

-- | Defines how attachment metadata fields should be mapped to index fields.
-- Before you can map a field, you must first create an index field with a
-- matching type using the console or the @UpdateIndex@ operation.
--
-- If you specify the @AttachentFieldMappings@ parameter, you must specify
-- at least one field mapping.
confluenceAttachmentConfiguration_attachmentFieldMappings :: Lens.Lens' ConfluenceAttachmentConfiguration (Prelude.Maybe (Prelude.NonEmpty ConfluenceAttachmentToIndexFieldMapping))
confluenceAttachmentConfiguration_attachmentFieldMappings = Lens.lens (\ConfluenceAttachmentConfiguration' {attachmentFieldMappings} -> attachmentFieldMappings) (\s@ConfluenceAttachmentConfiguration' {} a -> s {attachmentFieldMappings = a} :: ConfluenceAttachmentConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    ConfluenceAttachmentConfiguration
  where
  parseJSON =
    Core.withObject
      "ConfluenceAttachmentConfiguration"
      ( \x ->
          ConfluenceAttachmentConfiguration'
            Prelude.<$> (x Core..:? "CrawlAttachments")
            Prelude.<*> (x Core..:? "AttachmentFieldMappings")
      )

instance
  Prelude.Hashable
    ConfluenceAttachmentConfiguration

instance
  Prelude.NFData
    ConfluenceAttachmentConfiguration

instance
  Core.ToJSON
    ConfluenceAttachmentConfiguration
  where
  toJSON ConfluenceAttachmentConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CrawlAttachments" Core..=)
              Prelude.<$> crawlAttachments,
            ("AttachmentFieldMappings" Core..=)
              Prelude.<$> attachmentFieldMappings
          ]
      )
