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
-- Module      : Amazonka.Kendra.Types.WorkDocsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.WorkDocsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information to connect to Amazon WorkDocs as
-- your data source.
--
-- Amazon WorkDocs connector is available in Oregon, North Virginia,
-- Sydney, Singapore and Ireland regions.
--
-- /See:/ 'newWorkDocsConfiguration' smart constructor.
data WorkDocsConfiguration = WorkDocsConfiguration'
  { -- | @TRUE@ to include comments on documents in your index. Including
    -- comments in your index means each comment is a document that can be
    -- searched on.
    --
    -- The default is set to @FALSE@.
    crawlComments :: Prelude.Maybe Prelude.Bool,
    -- | A list of regular expression patterns to exclude certain files in your
    -- Amazon WorkDocs site repository. Files that match the patterns are
    -- excluded from the index. Files that don’t match the patterns are
    -- included in the index. If a file matches both an inclusion and exclusion
    -- pattern, the exclusion pattern takes precedence and the file isn\'t
    -- included in the index.
    exclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | A list of @DataSourceToIndexFieldMapping@ objects that map Amazon
    -- WorkDocs data source attributes or field names to Amazon Kendra index
    -- field names. To create custom fields, use the @UpdateIndex@ API before
    -- you map to Amazon WorkDocs fields. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
    -- The Amazon WorkDocs data source field names must exist in your Amazon
    -- WorkDocs custom metadata.
    fieldMappings :: Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping),
    -- | A list of regular expression patterns to include certain files in your
    -- Amazon WorkDocs site repository. Files that match the patterns are
    -- included in the index. Files that don\'t match the patterns are excluded
    -- from the index. If a file matches both an inclusion and exclusion
    -- pattern, the exclusion pattern takes precedence and the file isn\'t
    -- included in the index.
    inclusionPatterns :: Prelude.Maybe [Prelude.Text],
    -- | @TRUE@ to use the Amazon WorkDocs change log to determine which
    -- documents require updating in the index. Depending on the change log\'s
    -- size, it may take longer for Amazon Kendra to use the change log than to
    -- scan all of your documents in Amazon WorkDocs.
    useChangeLog :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the directory corresponding to your Amazon WorkDocs
    -- site repository.
    --
    -- You can find the organization ID in the
    -- <https://console.aws.amazon.com/directoryservicev2/ Directory Service>
    -- by going to __Active Directory__, then __Directories__. Your Amazon
    -- WorkDocs site directory has an ID, which is the organization ID. You can
    -- also set up a new Amazon WorkDocs directory in the Directory Service
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
-- 'crawlComments', 'workDocsConfiguration_crawlComments' - @TRUE@ to include comments on documents in your index. Including
-- comments in your index means each comment is a document that can be
-- searched on.
--
-- The default is set to @FALSE@.
--
-- 'exclusionPatterns', 'workDocsConfiguration_exclusionPatterns' - A list of regular expression patterns to exclude certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- excluded from the index. Files that don’t match the patterns are
-- included in the index. If a file matches both an inclusion and exclusion
-- pattern, the exclusion pattern takes precedence and the file isn\'t
-- included in the index.
--
-- 'fieldMappings', 'workDocsConfiguration_fieldMappings' - A list of @DataSourceToIndexFieldMapping@ objects that map Amazon
-- WorkDocs data source attributes or field names to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to Amazon WorkDocs fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Amazon WorkDocs data source field names must exist in your Amazon
-- WorkDocs custom metadata.
--
-- 'inclusionPatterns', 'workDocsConfiguration_inclusionPatterns' - A list of regular expression patterns to include certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- included in the index. Files that don\'t match the patterns are excluded
-- from the index. If a file matches both an inclusion and exclusion
-- pattern, the exclusion pattern takes precedence and the file isn\'t
-- included in the index.
--
-- 'useChangeLog', 'workDocsConfiguration_useChangeLog' - @TRUE@ to use the Amazon WorkDocs change log to determine which
-- documents require updating in the index. Depending on the change log\'s
-- size, it may take longer for Amazon Kendra to use the change log than to
-- scan all of your documents in Amazon WorkDocs.
--
-- 'organizationId', 'workDocsConfiguration_organizationId' - The identifier of the directory corresponding to your Amazon WorkDocs
-- site repository.
--
-- You can find the organization ID in the
-- <https://console.aws.amazon.com/directoryservicev2/ Directory Service>
-- by going to __Active Directory__, then __Directories__. Your Amazon
-- WorkDocs site directory has an ID, which is the organization ID. You can
-- also set up a new Amazon WorkDocs directory in the Directory Service
-- console and enable a Amazon WorkDocs site for the directory in the
-- Amazon WorkDocs console.
newWorkDocsConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  WorkDocsConfiguration
newWorkDocsConfiguration pOrganizationId_ =
  WorkDocsConfiguration'
    { crawlComments =
        Prelude.Nothing,
      exclusionPatterns = Prelude.Nothing,
      fieldMappings = Prelude.Nothing,
      inclusionPatterns = Prelude.Nothing,
      useChangeLog = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | @TRUE@ to include comments on documents in your index. Including
-- comments in your index means each comment is a document that can be
-- searched on.
--
-- The default is set to @FALSE@.
workDocsConfiguration_crawlComments :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe Prelude.Bool)
workDocsConfiguration_crawlComments = Lens.lens (\WorkDocsConfiguration' {crawlComments} -> crawlComments) (\s@WorkDocsConfiguration' {} a -> s {crawlComments = a} :: WorkDocsConfiguration)

-- | A list of regular expression patterns to exclude certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- excluded from the index. Files that don’t match the patterns are
-- included in the index. If a file matches both an inclusion and exclusion
-- pattern, the exclusion pattern takes precedence and the file isn\'t
-- included in the index.
workDocsConfiguration_exclusionPatterns :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe [Prelude.Text])
workDocsConfiguration_exclusionPatterns = Lens.lens (\WorkDocsConfiguration' {exclusionPatterns} -> exclusionPatterns) (\s@WorkDocsConfiguration' {} a -> s {exclusionPatterns = a} :: WorkDocsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of @DataSourceToIndexFieldMapping@ objects that map Amazon
-- WorkDocs data source attributes or field names to Amazon Kendra index
-- field names. To create custom fields, use the @UpdateIndex@ API before
-- you map to Amazon WorkDocs fields. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/field-mapping.html Mapping data source fields>.
-- The Amazon WorkDocs data source field names must exist in your Amazon
-- WorkDocs custom metadata.
workDocsConfiguration_fieldMappings :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe (Prelude.NonEmpty DataSourceToIndexFieldMapping))
workDocsConfiguration_fieldMappings = Lens.lens (\WorkDocsConfiguration' {fieldMappings} -> fieldMappings) (\s@WorkDocsConfiguration' {} a -> s {fieldMappings = a} :: WorkDocsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of regular expression patterns to include certain files in your
-- Amazon WorkDocs site repository. Files that match the patterns are
-- included in the index. Files that don\'t match the patterns are excluded
-- from the index. If a file matches both an inclusion and exclusion
-- pattern, the exclusion pattern takes precedence and the file isn\'t
-- included in the index.
workDocsConfiguration_inclusionPatterns :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe [Prelude.Text])
workDocsConfiguration_inclusionPatterns = Lens.lens (\WorkDocsConfiguration' {inclusionPatterns} -> inclusionPatterns) (\s@WorkDocsConfiguration' {} a -> s {inclusionPatterns = a} :: WorkDocsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | @TRUE@ to use the Amazon WorkDocs change log to determine which
-- documents require updating in the index. Depending on the change log\'s
-- size, it may take longer for Amazon Kendra to use the change log than to
-- scan all of your documents in Amazon WorkDocs.
workDocsConfiguration_useChangeLog :: Lens.Lens' WorkDocsConfiguration (Prelude.Maybe Prelude.Bool)
workDocsConfiguration_useChangeLog = Lens.lens (\WorkDocsConfiguration' {useChangeLog} -> useChangeLog) (\s@WorkDocsConfiguration' {} a -> s {useChangeLog = a} :: WorkDocsConfiguration)

-- | The identifier of the directory corresponding to your Amazon WorkDocs
-- site repository.
--
-- You can find the organization ID in the
-- <https://console.aws.amazon.com/directoryservicev2/ Directory Service>
-- by going to __Active Directory__, then __Directories__. Your Amazon
-- WorkDocs site directory has an ID, which is the organization ID. You can
-- also set up a new Amazon WorkDocs directory in the Directory Service
-- console and enable a Amazon WorkDocs site for the directory in the
-- Amazon WorkDocs console.
workDocsConfiguration_organizationId :: Lens.Lens' WorkDocsConfiguration Prelude.Text
workDocsConfiguration_organizationId = Lens.lens (\WorkDocsConfiguration' {organizationId} -> organizationId) (\s@WorkDocsConfiguration' {} a -> s {organizationId = a} :: WorkDocsConfiguration)

instance Data.FromJSON WorkDocsConfiguration where
  parseJSON =
    Data.withObject
      "WorkDocsConfiguration"
      ( \x ->
          WorkDocsConfiguration'
            Prelude.<$> (x Data..:? "CrawlComments")
            Prelude.<*> ( x
                            Data..:? "ExclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FieldMappings")
            Prelude.<*> ( x
                            Data..:? "InclusionPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UseChangeLog")
            Prelude.<*> (x Data..: "OrganizationId")
      )

instance Prelude.Hashable WorkDocsConfiguration where
  hashWithSalt _salt WorkDocsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` crawlComments
      `Prelude.hashWithSalt` exclusionPatterns
      `Prelude.hashWithSalt` fieldMappings
      `Prelude.hashWithSalt` inclusionPatterns
      `Prelude.hashWithSalt` useChangeLog
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData WorkDocsConfiguration where
  rnf WorkDocsConfiguration' {..} =
    Prelude.rnf crawlComments
      `Prelude.seq` Prelude.rnf exclusionPatterns
      `Prelude.seq` Prelude.rnf fieldMappings
      `Prelude.seq` Prelude.rnf inclusionPatterns
      `Prelude.seq` Prelude.rnf useChangeLog
      `Prelude.seq` Prelude.rnf organizationId

instance Data.ToJSON WorkDocsConfiguration where
  toJSON WorkDocsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CrawlComments" Data..=) Prelude.<$> crawlComments,
            ("ExclusionPatterns" Data..=)
              Prelude.<$> exclusionPatterns,
            ("FieldMappings" Data..=) Prelude.<$> fieldMappings,
            ("InclusionPatterns" Data..=)
              Prelude.<$> inclusionPatterns,
            ("UseChangeLog" Data..=) Prelude.<$> useChangeLog,
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )
