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
-- Module      : Network.AWS.Kendra.Types.WorkDocsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.WorkDocsConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the configuration information to connect to Amazon WorkDocs as
-- your data source.
--
-- Amazon WorkDocs connector is available in Oregon, North Virginia,
-- Sydney, Singapore and Ireland regions.
--
-- /See:/ 'newWorkDocsConfiguration' smart constructor.
data WorkDocsConfiguration = WorkDocsConfiguration'
  { -- | A list of @DataSourceToIndexFieldMapping@ objects that map Amazon
    -- WorkDocs field names to custom index field names in Amazon Kendra. You
    -- must first create the custom index fields using the @UpdateIndex@
    -- operation before you map to Amazon WorkDocs fields. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping Data Source Fields>.
    -- The Amazon WorkDocs data source field names need to exist in your Amazon
    -- WorkDocs custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | @TRUE@ to include comments on documents in your index. Including
    -- comments in your index means each comment is a document that can be
    -- searched on.
    --
    -- The default is set to @FALSE@.
    crawlComments :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ to use the change logs to update documents in your index instead
    -- of scanning all documents.
    --
    -- If you are syncing your Amazon WorkDocs data source with your index for
    -- the first time, all documents are scanned. After your first sync, you
    -- can use the change logs to update your documents in your index for
    -- future syncs.
    --
    -- The default is set to @FALSE@.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to exclude certain files in your
    -- Amazon WorkDocs site repository. Files that match the patterns are
    -- excluded from the index. Files that don’t match the patterns are
    -- included in the index. If a file matches both an inclusion pattern and
    -- an exclusion pattern, the exclusion pattern takes precedence and the
    -- file isn’t included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of regular expression patterns to include certain files in your
    -- Amazon WorkDocs site repository. Files that match the patterns are
    -- included in the index. Files that don\'t match the patterns are excluded
    -- from the index. If a file matches both an inclusion pattern and an
    -- exclusion pattern, the exclusion pattern takes precedence and the file
    -- isn’t included in the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the directory corresponding to your Amazon WorkDocs
    -- site repository.
    --
    -- You can find the organization ID in the
    -- <https://console.aws.amazon.com/directoryservicev2/ AWS Directory Service>
    -- by going to __Active Directory__, then __Directories__. Your Amazon
    -- WorkDocs site directory has an ID, which is the organization ID. You can
    -- also set up a new Amazon WorkDocs directory in the AWS Directory Service
    -- console and enable a Amazon WorkDocs site for the directory in the
    -- Amazon WorkDocs console.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkDocsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldMappings', 'workDocsConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map Amazon
-- WorkDocs field names to custom index field names in Amazon Kendra. You
-- must first create the custom index fields using the @UpdateIndex@
-- operation before you map to Amazon WorkDocs fields. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping Data Source Fields>.
-- The Amazon WorkDocs data source field names need to exist in your Amazon
-- WorkDocs custom metadata.
--
-- 'crawlComments', 'workDocsConfiguration_crawlComments' - @TRUE@ to include comments on documents in your index. Including
-- comments in your index means each comment is a document that can be
-- searched on.
--
-- The default is set to @FALSE@.
--
-- 'useChangeLog', 'workDocsConfiguration_useChangeLog' - @TRUE@ to use the change logs to update documents in your index instead
-- of scanning all documents.
--
-- If you are syncing your Amazon WorkDocs data source with your index for
-- the first time, all documents are scanned. After your first sync, you
-- can use the change logs to update your documents in your index for
-- future syncs.
--
-- The default is set to @FALSE@.
--
-- 'exclusionPatterns', 'workDocsConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- excluded from the index. Files that don’t match the patterns are
-- included in the index. If a file matches both an inclusion pattern and
-- an exclusion pattern, the exclusion pattern takes precedence and the
-- file isn’t included in the index.
--
-- 'inclusionPatterns', 'workDocsConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- included in the index. Files that don\'t match the patterns are excluded
-- from the index. If a file matches both an inclusion pattern and an
-- exclusion pattern, the exclusion pattern takes precedence and the file
-- isn’t included in the index.
--
-- 'organizationId', 'workDocsConfiguration_organizationId' - The identifier of the directory corresponding to your Amazon WorkDocs
-- site repository.
--
-- You can find the organization ID in the
-- <https://console.aws.amazon.com/directoryservicev2/ AWS Directory Service>
-- by going to __Active Directory__, then __Directories__. Your Amazon
-- WorkDocs site directory has an ID, which is the organization ID. You can
-- also set up a new Amazon WorkDocs directory in the AWS Directory Service
-- console and enable a Amazon WorkDocs site for the directory in the
-- Amazon WorkDocs console.
newWorkDocsConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  WorkDocsConfiguration
newWorkDocsConfiguration pOrganizationId_ =
  WorkDocsConfiguration'
    { fieldMappings =
        Prelude.Nothing,
      crawlComments = Prelude.Nothing,
      useChangeLog = Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | A list of @DataSourceToIndexFieldMapping@ objects that map Amazon
-- WorkDocs field names to custom index field names in Amazon Kendra. You
-- must first create the custom index fields using the @UpdateIndex@
-- operation before you map to Amazon WorkDocs fields. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping Data Source Fields>.
-- The Amazon WorkDocs data source field names need to exist in your Amazon
-- WorkDocs custom metadata.
workDocsConfiguration_fieldMappings :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
workDocsConfiguration_fieldMappings = Lens.lens (\WorkDocsConfiguration' {fieldMappings} -> fieldMappings) (\s@WorkDocsConfiguration' {} a -> s {fieldMappings = a} :: WorkDocsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to include comments on documents in your index. Including
-- comments in your index means each comment is a document that can be
-- searched on.
--
-- The default is set to @FALSE@.
workDocsConfiguration_crawlComments :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe Prelude.Bool)
workDocsConfiguration_crawlComments = Lens.lens (\WorkDocsConfiguration' {crawlComments} -> crawlComments) (\s@WorkDocsConfiguration' {} a -> s {crawlComments = a} :: WorkDocsConfiguration)

-- | @TRUE@ to use the change logs to update documents in your index instead
-- of scanning all documents.
--
-- If you are syncing your Amazon WorkDocs data source with your index for
-- the first time, all documents are scanned. After your first sync, you
-- can use the change logs to update your documents in your index for
-- future syncs.
--
-- The default is set to @FALSE@.
workDocsConfiguration_useChangeLog :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe Prelude.Bool)
workDocsConfiguration_useChangeLog = Lens.lens (\WorkDocsConfiguration' {useChangeLog} -> useChangeLog) (\s@WorkDocsConfiguration' {} a -> s {useChangeLog = a} :: WorkDocsConfiguration)

-- | A list of regular expression patterns to exclude certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- excluded from the index. Files that don’t match the patterns are
-- included in the index. If a file matches both an inclusion pattern and
-- an exclusion pattern, the exclusion pattern takes precedence and the
-- file isn’t included in the index.
workDocsConfiguration_exclusionPatterns :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe [Prelude.Text])
workDocsConfiguration_exclusionPatterns = Lens.lens (\WorkDocsConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@WorkDocsConfiguration' {} a -> s {exclusionPatterns = a} :: WorkDocsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- included in the index. Files that don\'t match the patterns are excluded
-- from the index. If a file matches both an inclusion pattern and an
-- exclusion pattern, the exclusion pattern takes precedence and the file
-- isn’t included in the index.
workDocsConfiguration_inclusionPatterns :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe [Prelude.Text])
workDocsConfiguration_inclusionPatterns = Lens.lens (\WorkDocsConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@WorkDocsConfiguration' {} a -> s {inclusionPatterns = a} :: WorkDocsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the directory corresponding to your Amazon WorkDocs
-- site repository.
--
-- You can find the organization ID in the
-- <https://console.aws.amazon.com/directoryservicev2/ AWS Directory Service>
-- by going to __Active Directory__, then __Directories__. Your Amazon
-- WorkDocs site directory has an ID, which is the organization ID. You can
-- also set up a new Amazon WorkDocs directory in the AWS Directory Service
-- console and enable a Amazon WorkDocs site for the directory in the
-- Amazon WorkDocs console.
workDocsConfiguration_organizationId :: Lens.Lens' WorkDocsConfiguration Prelude.Text
workDocsConfiguration_organizationId = Lens.lens (\WorkDocsConfiguration' {organizationId} -> organizationId) (\s@WorkDocsConfiguration' {} a -> s {organizationId = a} :: WorkDocsConfiguration)

instance Core.FromJSON WorkDocsConfiguration where
  parseJSON =
    Core.withObject
      "WorkDocsConfiguration"
      ( \x ->
          WorkDocsConfiguration'
            Prelude.<$> (x Core..:? "FieldMappings")
            Prelude.<*> (x Core..:? "CrawlComments")
            Prelude.<*> (x Core..:? "UseChangeLog")
            Prelude.<*> ( x Core..:? "ExclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InclusionPatterns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "OrganizationId")
      )

instance Prelude.Hashable WorkDocsConfiguration

instance Prelude.NFData WorkDocsConfiguration

instance Core.ToJSON WorkDocsConfiguration where
  toJSON WorkDocsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FieldMappings" Core..=) Prelude.<$> fieldMappings,
            ("CrawlComments" Core..=) Prelude.<$> crawlComments,
            ("UseChangeLog" Core..=) Prelude.<$> useChangeLog,
            ("ExclusionPatterns" Core..=)
              Prelude.<$> exclusionPatterns,
            ("InclusionPatterns" Core..=)
              Prelude.<$> inclusionPatterns,
            Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )
