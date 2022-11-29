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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Crawler where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | For scheduled crawlers, the schedule when the crawler runs.
    schedule :: Prelude.Maybe Schedule,
    -- | The name of the crawler.
    name :: Prelude.Maybe Prelude.Text,
    -- | A policy that specifies whether to crawl the entire dataset again, or to
    -- crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Prelude.Maybe RecrawlPolicy,
    -- | A list of UTF-8 strings that specify the custom classifiers that are
    -- associated with the crawler.
    classifiers :: Prelude.Maybe [Prelude.Text],
    -- | The policy that specifies update and delete behaviors for the crawler.
    schemaChangePolicy :: Prelude.Maybe SchemaChangePolicy,
    -- | The name of the database in which the crawler\'s output is stored.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Crawler configuration information. This versioned JSON string allows
    -- users to specify aspects of a crawler\'s behavior. For more information,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the crawler is running, or whether a run is pending.
    state :: Prelude.Maybe CrawlerState,
    -- | The prefix added to the names of tables that are created.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | A collection of targets to crawl.
    targets :: Prelude.Maybe CrawlerTargets,
    -- | A description of the crawler.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time that the crawler was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | If the crawler is running, contains the total time elapsed since the
    -- last crawl began.
    crawlElapsedTime :: Prelude.Maybe Prelude.Integer,
    -- | A configuration that specifies whether data lineage is enabled for the
    -- crawler.
    lineageConfiguration :: Prelude.Maybe LineageConfiguration,
    -- | The Amazon Resource Name (ARN) of an IAM role that\'s used to access
    -- customer resources, such as Amazon Simple Storage Service (Amazon S3)
    -- data.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The time that the crawler was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the last crawl, and potentially error information if an
    -- error occurred.
    lastCrawl :: Prelude.Maybe LastCrawlInfo,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the crawler should use Lake Formation credentials for
    -- the crawler instead of the IAM role credentials.
    lakeFormationConfiguration :: Prelude.Maybe LakeFormationConfiguration,
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
-- 'schedule', 'crawler_schedule' - For scheduled crawlers, the schedule when the crawler runs.
--
-- 'name', 'crawler_name' - The name of the crawler.
--
-- 'recrawlPolicy', 'crawler_recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
--
-- 'classifiers', 'crawler_classifiers' - A list of UTF-8 strings that specify the custom classifiers that are
-- associated with the crawler.
--
-- 'schemaChangePolicy', 'crawler_schemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
--
-- 'databaseName', 'crawler_databaseName' - The name of the database in which the crawler\'s output is stored.
--
-- 'configuration', 'crawler_configuration' - Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
--
-- 'state', 'crawler_state' - Indicates whether the crawler is running, or whether a run is pending.
--
-- 'tablePrefix', 'crawler_tablePrefix' - The prefix added to the names of tables that are created.
--
-- 'targets', 'crawler_targets' - A collection of targets to crawl.
--
-- 'description', 'crawler_description' - A description of the crawler.
--
-- 'lastUpdated', 'crawler_lastUpdated' - The time that the crawler was last updated.
--
-- 'crawlElapsedTime', 'crawler_crawlElapsedTime' - If the crawler is running, contains the total time elapsed since the
-- last crawl began.
--
-- 'lineageConfiguration', 'crawler_lineageConfiguration' - A configuration that specifies whether data lineage is enabled for the
-- crawler.
--
-- 'role'', 'crawler_role' - The Amazon Resource Name (ARN) of an IAM role that\'s used to access
-- customer resources, such as Amazon Simple Storage Service (Amazon S3)
-- data.
--
-- 'creationTime', 'crawler_creationTime' - The time that the crawler was created.
--
-- 'lastCrawl', 'crawler_lastCrawl' - The status of the last crawl, and potentially error information if an
-- error occurred.
--
-- 'crawlerSecurityConfiguration', 'crawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'lakeFormationConfiguration', 'crawler_lakeFormationConfiguration' - Specifies whether the crawler should use Lake Formation credentials for
-- the crawler instead of the IAM role credentials.
--
-- 'version', 'crawler_version' - The version of the crawler.
newCrawler ::
  Crawler
newCrawler =
  Crawler'
    { schedule = Prelude.Nothing,
      name = Prelude.Nothing,
      recrawlPolicy = Prelude.Nothing,
      classifiers = Prelude.Nothing,
      schemaChangePolicy = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      configuration = Prelude.Nothing,
      state = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      crawlElapsedTime = Prelude.Nothing,
      lineageConfiguration = Prelude.Nothing,
      role' = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastCrawl = Prelude.Nothing,
      crawlerSecurityConfiguration = Prelude.Nothing,
      lakeFormationConfiguration = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | For scheduled crawlers, the schedule when the crawler runs.
crawler_schedule :: Lens.Lens' Crawler (Prelude.Maybe Schedule)
crawler_schedule = Lens.lens (\Crawler' {schedule} -> schedule) (\s@Crawler' {} a -> s {schedule = a} :: Crawler)

-- | The name of the crawler.
crawler_name :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_name = Lens.lens (\Crawler' {name} -> name) (\s@Crawler' {} a -> s {name = a} :: Crawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
crawler_recrawlPolicy :: Lens.Lens' Crawler (Prelude.Maybe RecrawlPolicy)
crawler_recrawlPolicy = Lens.lens (\Crawler' {recrawlPolicy} -> recrawlPolicy) (\s@Crawler' {} a -> s {recrawlPolicy = a} :: Crawler)

-- | A list of UTF-8 strings that specify the custom classifiers that are
-- associated with the crawler.
crawler_classifiers :: Lens.Lens' Crawler (Prelude.Maybe [Prelude.Text])
crawler_classifiers = Lens.lens (\Crawler' {classifiers} -> classifiers) (\s@Crawler' {} a -> s {classifiers = a} :: Crawler) Prelude.. Lens.mapping Lens.coerced

-- | The policy that specifies update and delete behaviors for the crawler.
crawler_schemaChangePolicy :: Lens.Lens' Crawler (Prelude.Maybe SchemaChangePolicy)
crawler_schemaChangePolicy = Lens.lens (\Crawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@Crawler' {} a -> s {schemaChangePolicy = a} :: Crawler)

-- | The name of the database in which the crawler\'s output is stored.
crawler_databaseName :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_databaseName = Lens.lens (\Crawler' {databaseName} -> databaseName) (\s@Crawler' {} a -> s {databaseName = a} :: Crawler)

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
crawler_configuration :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_configuration = Lens.lens (\Crawler' {configuration} -> configuration) (\s@Crawler' {} a -> s {configuration = a} :: Crawler)

-- | Indicates whether the crawler is running, or whether a run is pending.
crawler_state :: Lens.Lens' Crawler (Prelude.Maybe CrawlerState)
crawler_state = Lens.lens (\Crawler' {state} -> state) (\s@Crawler' {} a -> s {state = a} :: Crawler)

-- | The prefix added to the names of tables that are created.
crawler_tablePrefix :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_tablePrefix = Lens.lens (\Crawler' {tablePrefix} -> tablePrefix) (\s@Crawler' {} a -> s {tablePrefix = a} :: Crawler)

-- | A collection of targets to crawl.
crawler_targets :: Lens.Lens' Crawler (Prelude.Maybe CrawlerTargets)
crawler_targets = Lens.lens (\Crawler' {targets} -> targets) (\s@Crawler' {} a -> s {targets = a} :: Crawler)

-- | A description of the crawler.
crawler_description :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_description = Lens.lens (\Crawler' {description} -> description) (\s@Crawler' {} a -> s {description = a} :: Crawler)

-- | The time that the crawler was last updated.
crawler_lastUpdated :: Lens.Lens' Crawler (Prelude.Maybe Prelude.UTCTime)
crawler_lastUpdated = Lens.lens (\Crawler' {lastUpdated} -> lastUpdated) (\s@Crawler' {} a -> s {lastUpdated = a} :: Crawler) Prelude.. Lens.mapping Core._Time

-- | If the crawler is running, contains the total time elapsed since the
-- last crawl began.
crawler_crawlElapsedTime :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Integer)
crawler_crawlElapsedTime = Lens.lens (\Crawler' {crawlElapsedTime} -> crawlElapsedTime) (\s@Crawler' {} a -> s {crawlElapsedTime = a} :: Crawler)

-- | A configuration that specifies whether data lineage is enabled for the
-- crawler.
crawler_lineageConfiguration :: Lens.Lens' Crawler (Prelude.Maybe LineageConfiguration)
crawler_lineageConfiguration = Lens.lens (\Crawler' {lineageConfiguration} -> lineageConfiguration) (\s@Crawler' {} a -> s {lineageConfiguration = a} :: Crawler)

-- | The Amazon Resource Name (ARN) of an IAM role that\'s used to access
-- customer resources, such as Amazon Simple Storage Service (Amazon S3)
-- data.
crawler_role :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_role = Lens.lens (\Crawler' {role'} -> role') (\s@Crawler' {} a -> s {role' = a} :: Crawler)

-- | The time that the crawler was created.
crawler_creationTime :: Lens.Lens' Crawler (Prelude.Maybe Prelude.UTCTime)
crawler_creationTime = Lens.lens (\Crawler' {creationTime} -> creationTime) (\s@Crawler' {} a -> s {creationTime = a} :: Crawler) Prelude.. Lens.mapping Core._Time

-- | The status of the last crawl, and potentially error information if an
-- error occurred.
crawler_lastCrawl :: Lens.Lens' Crawler (Prelude.Maybe LastCrawlInfo)
crawler_lastCrawl = Lens.lens (\Crawler' {lastCrawl} -> lastCrawl) (\s@Crawler' {} a -> s {lastCrawl = a} :: Crawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
crawler_crawlerSecurityConfiguration :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_crawlerSecurityConfiguration = Lens.lens (\Crawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@Crawler' {} a -> s {crawlerSecurityConfiguration = a} :: Crawler)

-- | Specifies whether the crawler should use Lake Formation credentials for
-- the crawler instead of the IAM role credentials.
crawler_lakeFormationConfiguration :: Lens.Lens' Crawler (Prelude.Maybe LakeFormationConfiguration)
crawler_lakeFormationConfiguration = Lens.lens (\Crawler' {lakeFormationConfiguration} -> lakeFormationConfiguration) (\s@Crawler' {} a -> s {lakeFormationConfiguration = a} :: Crawler)

-- | The version of the crawler.
crawler_version :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Integer)
crawler_version = Lens.lens (\Crawler' {version} -> version) (\s@Crawler' {} a -> s {version = a} :: Crawler)

instance Core.FromJSON Crawler where
  parseJSON =
    Core.withObject
      "Crawler"
      ( \x ->
          Crawler'
            Prelude.<$> (x Core..:? "Schedule")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RecrawlPolicy")
            Prelude.<*> (x Core..:? "Classifiers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SchemaChangePolicy")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "TablePrefix")
            Prelude.<*> (x Core..:? "Targets")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "CrawlElapsedTime")
            Prelude.<*> (x Core..:? "LineageConfiguration")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LastCrawl")
            Prelude.<*> (x Core..:? "CrawlerSecurityConfiguration")
            Prelude.<*> (x Core..:? "LakeFormationConfiguration")
            Prelude.<*> (x Core..:? "Version")
      )

instance Prelude.Hashable Crawler where
  hashWithSalt _salt Crawler' {..} =
    _salt `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recrawlPolicy
      `Prelude.hashWithSalt` classifiers
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tablePrefix
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` crawlElapsedTime
      `Prelude.hashWithSalt` lineageConfiguration
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastCrawl
      `Prelude.hashWithSalt` crawlerSecurityConfiguration
      `Prelude.hashWithSalt` lakeFormationConfiguration
      `Prelude.hashWithSalt` version

instance Prelude.NFData Crawler where
  rnf Crawler' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recrawlPolicy
      `Prelude.seq` Prelude.rnf classifiers
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tablePrefix
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf crawlElapsedTime
      `Prelude.seq` Prelude.rnf lineageConfiguration
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastCrawl
      `Prelude.seq` Prelude.rnf
        crawlerSecurityConfiguration
      `Prelude.seq` Prelude.rnf
        lakeFormationConfiguration
      `Prelude.seq` Prelude.rnf version
