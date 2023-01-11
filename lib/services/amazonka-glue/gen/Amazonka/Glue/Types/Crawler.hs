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
-- Module      : Amazonka.Glue.Types.Crawler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Crawler where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CrawlerState
import Amazonka.Glue.Types.CrawlerTargets
import Amazonka.Glue.Types.LakeFormationConfiguration
import Amazonka.Glue.Types.LastCrawlInfo
import Amazonka.Glue.Types.LineageConfiguration
import Amazonka.Glue.Types.RecrawlPolicy
import Amazonka.Glue.Types.Schedule
import Amazonka.Glue.Types.SchemaChangePolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies a crawler program that examines a data source and uses
-- classifiers to try to determine its schema. If successful, the crawler
-- records metadata concerning the data source in the Glue Data Catalog.
--
-- /See:/ 'newCrawler' smart constructor.
data Crawler = Crawler'
  { -- | A list of UTF-8 strings that specify the custom classifiers that are
    -- associated with the crawler.
    classifiers :: Prelude.Maybe [Prelude.Text],
    -- | Crawler configuration information. This versioned JSON string allows
    -- users to specify aspects of a crawler\'s behavior. For more information,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | If the crawler is running, contains the total time elapsed since the
    -- last crawl began.
    crawlElapsedTime :: Prelude.Maybe Prelude.Integer,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The time that the crawler was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the database in which the crawler\'s output is stored.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A description of the crawler.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the crawler should use Lake Formation credentials for
    -- the crawler instead of the IAM role credentials.
    lakeFormationConfiguration :: Prelude.Maybe LakeFormationConfiguration,
    -- | The status of the last crawl, and potentially error information if an
    -- error occurred.
    lastCrawl :: Prelude.Maybe LastCrawlInfo,
    -- | The time that the crawler was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | A configuration that specifies whether data lineage is enabled for the
    -- crawler.
    lineageConfiguration :: Prelude.Maybe LineageConfiguration,
    -- | The name of the crawler.
    name :: Prelude.Maybe Prelude.Text,
    -- | A policy that specifies whether to crawl the entire dataset again, or to
    -- crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Prelude.Maybe RecrawlPolicy,
    -- | The Amazon Resource Name (ARN) of an IAM role that\'s used to access
    -- customer resources, such as Amazon Simple Storage Service (Amazon S3)
    -- data.
    role' :: Prelude.Maybe Prelude.Text,
    -- | For scheduled crawlers, the schedule when the crawler runs.
    schedule :: Prelude.Maybe Schedule,
    -- | The policy that specifies update and delete behaviors for the crawler.
    schemaChangePolicy :: Prelude.Maybe SchemaChangePolicy,
    -- | Indicates whether the crawler is running, or whether a run is pending.
    state :: Prelude.Maybe CrawlerState,
    -- | The prefix added to the names of tables that are created.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | A collection of targets to crawl.
    targets :: Prelude.Maybe CrawlerTargets,
    -- | The version of the crawler.
    version :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Crawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classifiers', 'crawler_classifiers' - A list of UTF-8 strings that specify the custom classifiers that are
-- associated with the crawler.
--
-- 'configuration', 'crawler_configuration' - Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
--
-- 'crawlElapsedTime', 'crawler_crawlElapsedTime' - If the crawler is running, contains the total time elapsed since the
-- last crawl began.
--
-- 'crawlerSecurityConfiguration', 'crawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'creationTime', 'crawler_creationTime' - The time that the crawler was created.
--
-- 'databaseName', 'crawler_databaseName' - The name of the database in which the crawler\'s output is stored.
--
-- 'description', 'crawler_description' - A description of the crawler.
--
-- 'lakeFormationConfiguration', 'crawler_lakeFormationConfiguration' - Specifies whether the crawler should use Lake Formation credentials for
-- the crawler instead of the IAM role credentials.
--
-- 'lastCrawl', 'crawler_lastCrawl' - The status of the last crawl, and potentially error information if an
-- error occurred.
--
-- 'lastUpdated', 'crawler_lastUpdated' - The time that the crawler was last updated.
--
-- 'lineageConfiguration', 'crawler_lineageConfiguration' - A configuration that specifies whether data lineage is enabled for the
-- crawler.
--
-- 'name', 'crawler_name' - The name of the crawler.
--
-- 'recrawlPolicy', 'crawler_recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
--
-- 'role'', 'crawler_role' - The Amazon Resource Name (ARN) of an IAM role that\'s used to access
-- customer resources, such as Amazon Simple Storage Service (Amazon S3)
-- data.
--
-- 'schedule', 'crawler_schedule' - For scheduled crawlers, the schedule when the crawler runs.
--
-- 'schemaChangePolicy', 'crawler_schemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
--
-- 'state', 'crawler_state' - Indicates whether the crawler is running, or whether a run is pending.
--
-- 'tablePrefix', 'crawler_tablePrefix' - The prefix added to the names of tables that are created.
--
-- 'targets', 'crawler_targets' - A collection of targets to crawl.
--
-- 'version', 'crawler_version' - The version of the crawler.
newCrawler ::
  Crawler
newCrawler =
  Crawler'
    { classifiers = Prelude.Nothing,
      configuration = Prelude.Nothing,
      crawlElapsedTime = Prelude.Nothing,
      crawlerSecurityConfiguration = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      description = Prelude.Nothing,
      lakeFormationConfiguration = Prelude.Nothing,
      lastCrawl = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      lineageConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      recrawlPolicy = Prelude.Nothing,
      role' = Prelude.Nothing,
      schedule = Prelude.Nothing,
      schemaChangePolicy = Prelude.Nothing,
      state = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      targets = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A list of UTF-8 strings that specify the custom classifiers that are
-- associated with the crawler.
crawler_classifiers :: Lens.Lens' Crawler (Prelude.Maybe [Prelude.Text])
crawler_classifiers = Lens.lens (\Crawler' {classifiers} -> classifiers) (\s@Crawler' {} a -> s {classifiers = a} :: Crawler) Prelude.. Lens.mapping Lens.coerced

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
crawler_configuration :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_configuration = Lens.lens (\Crawler' {configuration} -> configuration) (\s@Crawler' {} a -> s {configuration = a} :: Crawler)

-- | If the crawler is running, contains the total time elapsed since the
-- last crawl began.
crawler_crawlElapsedTime :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Integer)
crawler_crawlElapsedTime = Lens.lens (\Crawler' {crawlElapsedTime} -> crawlElapsedTime) (\s@Crawler' {} a -> s {crawlElapsedTime = a} :: Crawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
crawler_crawlerSecurityConfiguration :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_crawlerSecurityConfiguration = Lens.lens (\Crawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@Crawler' {} a -> s {crawlerSecurityConfiguration = a} :: Crawler)

-- | The time that the crawler was created.
crawler_creationTime :: Lens.Lens' Crawler (Prelude.Maybe Prelude.UTCTime)
crawler_creationTime = Lens.lens (\Crawler' {creationTime} -> creationTime) (\s@Crawler' {} a -> s {creationTime = a} :: Crawler) Prelude.. Lens.mapping Data._Time

-- | The name of the database in which the crawler\'s output is stored.
crawler_databaseName :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_databaseName = Lens.lens (\Crawler' {databaseName} -> databaseName) (\s@Crawler' {} a -> s {databaseName = a} :: Crawler)

-- | A description of the crawler.
crawler_description :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_description = Lens.lens (\Crawler' {description} -> description) (\s@Crawler' {} a -> s {description = a} :: Crawler)

-- | Specifies whether the crawler should use Lake Formation credentials for
-- the crawler instead of the IAM role credentials.
crawler_lakeFormationConfiguration :: Lens.Lens' Crawler (Prelude.Maybe LakeFormationConfiguration)
crawler_lakeFormationConfiguration = Lens.lens (\Crawler' {lakeFormationConfiguration} -> lakeFormationConfiguration) (\s@Crawler' {} a -> s {lakeFormationConfiguration = a} :: Crawler)

-- | The status of the last crawl, and potentially error information if an
-- error occurred.
crawler_lastCrawl :: Lens.Lens' Crawler (Prelude.Maybe LastCrawlInfo)
crawler_lastCrawl = Lens.lens (\Crawler' {lastCrawl} -> lastCrawl) (\s@Crawler' {} a -> s {lastCrawl = a} :: Crawler)

-- | The time that the crawler was last updated.
crawler_lastUpdated :: Lens.Lens' Crawler (Prelude.Maybe Prelude.UTCTime)
crawler_lastUpdated = Lens.lens (\Crawler' {lastUpdated} -> lastUpdated) (\s@Crawler' {} a -> s {lastUpdated = a} :: Crawler) Prelude.. Lens.mapping Data._Time

-- | A configuration that specifies whether data lineage is enabled for the
-- crawler.
crawler_lineageConfiguration :: Lens.Lens' Crawler (Prelude.Maybe LineageConfiguration)
crawler_lineageConfiguration = Lens.lens (\Crawler' {lineageConfiguration} -> lineageConfiguration) (\s@Crawler' {} a -> s {lineageConfiguration = a} :: Crawler)

-- | The name of the crawler.
crawler_name :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_name = Lens.lens (\Crawler' {name} -> name) (\s@Crawler' {} a -> s {name = a} :: Crawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
crawler_recrawlPolicy :: Lens.Lens' Crawler (Prelude.Maybe RecrawlPolicy)
crawler_recrawlPolicy = Lens.lens (\Crawler' {recrawlPolicy} -> recrawlPolicy) (\s@Crawler' {} a -> s {recrawlPolicy = a} :: Crawler)

-- | The Amazon Resource Name (ARN) of an IAM role that\'s used to access
-- customer resources, such as Amazon Simple Storage Service (Amazon S3)
-- data.
crawler_role :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_role = Lens.lens (\Crawler' {role'} -> role') (\s@Crawler' {} a -> s {role' = a} :: Crawler)

-- | For scheduled crawlers, the schedule when the crawler runs.
crawler_schedule :: Lens.Lens' Crawler (Prelude.Maybe Schedule)
crawler_schedule = Lens.lens (\Crawler' {schedule} -> schedule) (\s@Crawler' {} a -> s {schedule = a} :: Crawler)

-- | The policy that specifies update and delete behaviors for the crawler.
crawler_schemaChangePolicy :: Lens.Lens' Crawler (Prelude.Maybe SchemaChangePolicy)
crawler_schemaChangePolicy = Lens.lens (\Crawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@Crawler' {} a -> s {schemaChangePolicy = a} :: Crawler)

-- | Indicates whether the crawler is running, or whether a run is pending.
crawler_state :: Lens.Lens' Crawler (Prelude.Maybe CrawlerState)
crawler_state = Lens.lens (\Crawler' {state} -> state) (\s@Crawler' {} a -> s {state = a} :: Crawler)

-- | The prefix added to the names of tables that are created.
crawler_tablePrefix :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_tablePrefix = Lens.lens (\Crawler' {tablePrefix} -> tablePrefix) (\s@Crawler' {} a -> s {tablePrefix = a} :: Crawler)

-- | A collection of targets to crawl.
crawler_targets :: Lens.Lens' Crawler (Prelude.Maybe CrawlerTargets)
crawler_targets = Lens.lens (\Crawler' {targets} -> targets) (\s@Crawler' {} a -> s {targets = a} :: Crawler)

-- | The version of the crawler.
crawler_version :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Integer)
crawler_version = Lens.lens (\Crawler' {version} -> version) (\s@Crawler' {} a -> s {version = a} :: Crawler)

instance Data.FromJSON Crawler where
  parseJSON =
    Data.withObject
      "Crawler"
      ( \x ->
          Crawler'
            Prelude.<$> (x Data..:? "Classifiers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Configuration")
            Prelude.<*> (x Data..:? "CrawlElapsedTime")
            Prelude.<*> (x Data..:? "CrawlerSecurityConfiguration")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LakeFormationConfiguration")
            Prelude.<*> (x Data..:? "LastCrawl")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "LineageConfiguration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RecrawlPolicy")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "Schedule")
            Prelude.<*> (x Data..:? "SchemaChangePolicy")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "TablePrefix")
            Prelude.<*> (x Data..:? "Targets")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable Crawler where
  hashWithSalt _salt Crawler' {..} =
    _salt `Prelude.hashWithSalt` classifiers
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` crawlElapsedTime
      `Prelude.hashWithSalt` crawlerSecurityConfiguration
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lakeFormationConfiguration
      `Prelude.hashWithSalt` lastCrawl
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` lineageConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recrawlPolicy
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tablePrefix
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` version

instance Prelude.NFData Crawler where
  rnf Crawler' {..} =
    Prelude.rnf classifiers
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf crawlElapsedTime
      `Prelude.seq` Prelude.rnf crawlerSecurityConfiguration
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lakeFormationConfiguration
      `Prelude.seq` Prelude.rnf lastCrawl
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf lineageConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recrawlPolicy
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tablePrefix
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf version
