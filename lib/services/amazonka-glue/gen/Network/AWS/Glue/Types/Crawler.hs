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
-- Module      : Network.AWS.Glue.Types.Crawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Crawler where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CrawlerState
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.LineageConfiguration
import Network.AWS.Glue.Types.RecrawlPolicy
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.SchemaChangePolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a crawler program that examines a data source and uses
-- classifiers to try to determine its schema. If successful, the crawler
-- records metadata concerning the data source in the Glue Data Catalog.
--
-- /See:/ 'newCrawler' smart constructor.
data Crawler = Crawler'
  { -- | The time that the crawler was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the crawler is running, or whether a run is pending.
    state :: Prelude.Maybe CrawlerState,
    -- | The policy that specifies update and delete behaviors for the crawler.
    schemaChangePolicy :: Prelude.Maybe SchemaChangePolicy,
    -- | The time that the crawler was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | For scheduled crawlers, the schedule when the crawler runs.
    schedule :: Prelude.Maybe Schedule,
    -- | The status of the last crawl, and potentially error information if an
    -- error occurred.
    lastCrawl :: Prelude.Maybe LastCrawlInfo,
    -- | If the crawler is running, contains the total time elapsed since the
    -- last crawl began.
    crawlElapsedTime :: Prelude.Maybe Prelude.Integer,
    -- | A policy that specifies whether to crawl the entire dataset again, or to
    -- crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Prelude.Maybe RecrawlPolicy,
    -- | A list of UTF-8 strings that specify the custom classifiers that are
    -- associated with the crawler.
    classifiers :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of an IAM role that\'s used to access
    -- customer resources, such as Amazon Simple Storage Service (Amazon S3)
    -- data.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name of the crawler.
    name :: Prelude.Maybe Prelude.Text,
    -- | A collection of targets to crawl.
    targets :: Prelude.Maybe CrawlerTargets,
    -- | The version of the crawler.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the database in which the crawler\'s output is stored.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | A configuration that specifies whether data lineage is enabled for the
    -- crawler.
    lineageConfiguration :: Prelude.Maybe LineageConfiguration,
    -- | Crawler configuration information. This versioned JSON string allows
    -- users to specify aspects of a crawler\'s behavior. For more information,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/define-crawler.html#crawler-data-stores-exclude Include and Exclude Patterns>.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The prefix added to the names of tables that are created.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | A description of the crawler.
    description :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'crawler_creationTime' - The time that the crawler was created.
--
-- 'state', 'crawler_state' - Indicates whether the crawler is running, or whether a run is pending.
--
-- 'schemaChangePolicy', 'crawler_schemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
--
-- 'lastUpdated', 'crawler_lastUpdated' - The time that the crawler was last updated.
--
-- 'schedule', 'crawler_schedule' - For scheduled crawlers, the schedule when the crawler runs.
--
-- 'lastCrawl', 'crawler_lastCrawl' - The status of the last crawl, and potentially error information if an
-- error occurred.
--
-- 'crawlElapsedTime', 'crawler_crawlElapsedTime' - If the crawler is running, contains the total time elapsed since the
-- last crawl began.
--
-- 'recrawlPolicy', 'crawler_recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
--
-- 'classifiers', 'crawler_classifiers' - A list of UTF-8 strings that specify the custom classifiers that are
-- associated with the crawler.
--
-- 'role'', 'crawler_role' - The Amazon Resource Name (ARN) of an IAM role that\'s used to access
-- customer resources, such as Amazon Simple Storage Service (Amazon S3)
-- data.
--
-- 'name', 'crawler_name' - The name of the crawler.
--
-- 'targets', 'crawler_targets' - A collection of targets to crawl.
--
-- 'version', 'crawler_version' - The version of the crawler.
--
-- 'databaseName', 'crawler_databaseName' - The name of the database in which the crawler\'s output is stored.
--
-- 'crawlerSecurityConfiguration', 'crawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'lineageConfiguration', 'crawler_lineageConfiguration' - A configuration that specifies whether data lineage is enabled for the
-- crawler.
--
-- 'configuration', 'crawler_configuration' - Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/define-crawler.html#crawler-data-stores-exclude Include and Exclude Patterns>.
--
-- 'tablePrefix', 'crawler_tablePrefix' - The prefix added to the names of tables that are created.
--
-- 'description', 'crawler_description' - A description of the crawler.
newCrawler ::
  Crawler
newCrawler =
  Crawler'
    { creationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      schemaChangePolicy = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      schedule = Prelude.Nothing,
      lastCrawl = Prelude.Nothing,
      crawlElapsedTime = Prelude.Nothing,
      recrawlPolicy = Prelude.Nothing,
      classifiers = Prelude.Nothing,
      role' = Prelude.Nothing,
      name = Prelude.Nothing,
      targets = Prelude.Nothing,
      version = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      crawlerSecurityConfiguration = Prelude.Nothing,
      lineageConfiguration = Prelude.Nothing,
      configuration = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The time that the crawler was created.
crawler_creationTime :: Lens.Lens' Crawler (Prelude.Maybe Prelude.UTCTime)
crawler_creationTime = Lens.lens (\Crawler' {creationTime} -> creationTime) (\s@Crawler' {} a -> s {creationTime = a} :: Crawler) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the crawler is running, or whether a run is pending.
crawler_state :: Lens.Lens' Crawler (Prelude.Maybe CrawlerState)
crawler_state = Lens.lens (\Crawler' {state} -> state) (\s@Crawler' {} a -> s {state = a} :: Crawler)

-- | The policy that specifies update and delete behaviors for the crawler.
crawler_schemaChangePolicy :: Lens.Lens' Crawler (Prelude.Maybe SchemaChangePolicy)
crawler_schemaChangePolicy = Lens.lens (\Crawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@Crawler' {} a -> s {schemaChangePolicy = a} :: Crawler)

-- | The time that the crawler was last updated.
crawler_lastUpdated :: Lens.Lens' Crawler (Prelude.Maybe Prelude.UTCTime)
crawler_lastUpdated = Lens.lens (\Crawler' {lastUpdated} -> lastUpdated) (\s@Crawler' {} a -> s {lastUpdated = a} :: Crawler) Prelude.. Lens.mapping Core._Time

-- | For scheduled crawlers, the schedule when the crawler runs.
crawler_schedule :: Lens.Lens' Crawler (Prelude.Maybe Schedule)
crawler_schedule = Lens.lens (\Crawler' {schedule} -> schedule) (\s@Crawler' {} a -> s {schedule = a} :: Crawler)

-- | The status of the last crawl, and potentially error information if an
-- error occurred.
crawler_lastCrawl :: Lens.Lens' Crawler (Prelude.Maybe LastCrawlInfo)
crawler_lastCrawl = Lens.lens (\Crawler' {lastCrawl} -> lastCrawl) (\s@Crawler' {} a -> s {lastCrawl = a} :: Crawler)

-- | If the crawler is running, contains the total time elapsed since the
-- last crawl began.
crawler_crawlElapsedTime :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Integer)
crawler_crawlElapsedTime = Lens.lens (\Crawler' {crawlElapsedTime} -> crawlElapsedTime) (\s@Crawler' {} a -> s {crawlElapsedTime = a} :: Crawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
crawler_recrawlPolicy :: Lens.Lens' Crawler (Prelude.Maybe RecrawlPolicy)
crawler_recrawlPolicy = Lens.lens (\Crawler' {recrawlPolicy} -> recrawlPolicy) (\s@Crawler' {} a -> s {recrawlPolicy = a} :: Crawler)

-- | A list of UTF-8 strings that specify the custom classifiers that are
-- associated with the crawler.
crawler_classifiers :: Lens.Lens' Crawler (Prelude.Maybe [Prelude.Text])
crawler_classifiers = Lens.lens (\Crawler' {classifiers} -> classifiers) (\s@Crawler' {} a -> s {classifiers = a} :: Crawler) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role that\'s used to access
-- customer resources, such as Amazon Simple Storage Service (Amazon S3)
-- data.
crawler_role :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_role = Lens.lens (\Crawler' {role'} -> role') (\s@Crawler' {} a -> s {role' = a} :: Crawler)

-- | The name of the crawler.
crawler_name :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_name = Lens.lens (\Crawler' {name} -> name) (\s@Crawler' {} a -> s {name = a} :: Crawler)

-- | A collection of targets to crawl.
crawler_targets :: Lens.Lens' Crawler (Prelude.Maybe CrawlerTargets)
crawler_targets = Lens.lens (\Crawler' {targets} -> targets) (\s@Crawler' {} a -> s {targets = a} :: Crawler)

-- | The version of the crawler.
crawler_version :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Integer)
crawler_version = Lens.lens (\Crawler' {version} -> version) (\s@Crawler' {} a -> s {version = a} :: Crawler)

-- | The name of the database in which the crawler\'s output is stored.
crawler_databaseName :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_databaseName = Lens.lens (\Crawler' {databaseName} -> databaseName) (\s@Crawler' {} a -> s {databaseName = a} :: Crawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
crawler_crawlerSecurityConfiguration :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_crawlerSecurityConfiguration = Lens.lens (\Crawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@Crawler' {} a -> s {crawlerSecurityConfiguration = a} :: Crawler)

-- | A configuration that specifies whether data lineage is enabled for the
-- crawler.
crawler_lineageConfiguration :: Lens.Lens' Crawler (Prelude.Maybe LineageConfiguration)
crawler_lineageConfiguration = Lens.lens (\Crawler' {lineageConfiguration} -> lineageConfiguration) (\s@Crawler' {} a -> s {lineageConfiguration = a} :: Crawler)

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/define-crawler.html#crawler-data-stores-exclude Include and Exclude Patterns>.
crawler_configuration :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_configuration = Lens.lens (\Crawler' {configuration} -> configuration) (\s@Crawler' {} a -> s {configuration = a} :: Crawler)

-- | The prefix added to the names of tables that are created.
crawler_tablePrefix :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_tablePrefix = Lens.lens (\Crawler' {tablePrefix} -> tablePrefix) (\s@Crawler' {} a -> s {tablePrefix = a} :: Crawler)

-- | A description of the crawler.
crawler_description :: Lens.Lens' Crawler (Prelude.Maybe Prelude.Text)
crawler_description = Lens.lens (\Crawler' {description} -> description) (\s@Crawler' {} a -> s {description = a} :: Crawler)

instance Core.FromJSON Crawler where
  parseJSON =
    Core.withObject
      "Crawler"
      ( \x ->
          Crawler'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "SchemaChangePolicy")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "Schedule")
            Prelude.<*> (x Core..:? "LastCrawl")
            Prelude.<*> (x Core..:? "CrawlElapsedTime")
            Prelude.<*> (x Core..:? "RecrawlPolicy")
            Prelude.<*> (x Core..:? "Classifiers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Targets")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "CrawlerSecurityConfiguration")
            Prelude.<*> (x Core..:? "LineageConfiguration")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "TablePrefix")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable Crawler

instance Prelude.NFData Crawler
