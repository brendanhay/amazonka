{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Crawler
  ( Crawler (..),

    -- * Smart constructor
    mkCrawler,

    -- * Lenses
    ccCreationTime,
    ccState,
    ccSchemaChangePolicy,
    ccLastUpdated,
    ccSchedule,
    ccLastCrawl,
    ccCrawlElapsedTime,
    ccRecrawlPolicy,
    ccClassifiers,
    ccRole,
    ccName,
    ccTargets,
    ccVersion,
    ccDatabaseName,
    ccCrawlerSecurityConfiguration,
    ccLineageConfiguration,
    ccConfiguration,
    ccTablePrefix,
    ccDescription,
  )
where

import Network.AWS.Glue.Types.CrawlerState
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.LineageConfiguration
import Network.AWS.Glue.Types.RecrawlPolicy
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.SchemaChangePolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.
--
-- /See:/ 'mkCrawler' smart constructor.
data Crawler = Crawler'
  { creationTime :: Lude.Maybe Lude.Timestamp,
    state :: Lude.Maybe CrawlerState,
    schemaChangePolicy :: Lude.Maybe SchemaChangePolicy,
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    schedule :: Lude.Maybe Schedule,
    lastCrawl :: Lude.Maybe LastCrawlInfo,
    crawlElapsedTime :: Lude.Maybe Lude.Integer,
    recrawlPolicy :: Lude.Maybe RecrawlPolicy,
    classifiers :: Lude.Maybe [Lude.Text],
    role' :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe CrawlerTargets,
    version :: Lude.Maybe Lude.Integer,
    databaseName :: Lude.Maybe Lude.Text,
    crawlerSecurityConfiguration :: Lude.Maybe Lude.Text,
    lineageConfiguration :: Lude.Maybe LineageConfiguration,
    configuration :: Lude.Maybe Lude.Text,
    tablePrefix :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Crawler' with the minimum fields required to make a request.
--
-- * 'classifiers' - A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
-- * 'configuration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
-- * 'crawlElapsedTime' - If the crawler is running, contains the total time elapsed since the last crawl began.
-- * 'crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
-- * 'creationTime' - The time that the crawler was created.
-- * 'databaseName' - The name of the database in which the crawler's output is stored.
-- * 'description' - A description of the crawler.
-- * 'lastCrawl' - The status of the last crawl, and potentially error information if an error occurred.
-- * 'lastUpdated' - The time that the crawler was last updated.
-- * 'lineageConfiguration' - A configuration that specifies whether data lineage is enabled for the crawler.
-- * 'name' - The name of the crawler.
-- * 'recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
-- * 'role'' - The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
-- * 'schedule' - For scheduled crawlers, the schedule when the crawler runs.
-- * 'schemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
-- * 'state' - Indicates whether the crawler is running, or whether a run is pending.
-- * 'tablePrefix' - The prefix added to the names of tables that are created.
-- * 'targets' - A collection of targets to crawl.
-- * 'version' - The version of the crawler.
mkCrawler ::
  Crawler
mkCrawler =
  Crawler'
    { creationTime = Lude.Nothing,
      state = Lude.Nothing,
      schemaChangePolicy = Lude.Nothing,
      lastUpdated = Lude.Nothing,
      schedule = Lude.Nothing,
      lastCrawl = Lude.Nothing,
      crawlElapsedTime = Lude.Nothing,
      recrawlPolicy = Lude.Nothing,
      classifiers = Lude.Nothing,
      role' = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      version = Lude.Nothing,
      databaseName = Lude.Nothing,
      crawlerSecurityConfiguration = Lude.Nothing,
      lineageConfiguration = Lude.Nothing,
      configuration = Lude.Nothing,
      tablePrefix = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The time that the crawler was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCreationTime :: Lens.Lens' Crawler (Lude.Maybe Lude.Timestamp)
ccCreationTime = Lens.lens (creationTime :: Crawler -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Crawler)
{-# DEPRECATED ccCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Indicates whether the crawler is running, or whether a run is pending.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccState :: Lens.Lens' Crawler (Lude.Maybe CrawlerState)
ccState = Lens.lens (state :: Crawler -> Lude.Maybe CrawlerState) (\s a -> s {state = a} :: Crawler)
{-# DEPRECATED ccState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The policy that specifies update and delete behaviors for the crawler.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSchemaChangePolicy :: Lens.Lens' Crawler (Lude.Maybe SchemaChangePolicy)
ccSchemaChangePolicy = Lens.lens (schemaChangePolicy :: Crawler -> Lude.Maybe SchemaChangePolicy) (\s a -> s {schemaChangePolicy = a} :: Crawler)
{-# DEPRECATED ccSchemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead." #-}

-- | The time that the crawler was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLastUpdated :: Lens.Lens' Crawler (Lude.Maybe Lude.Timestamp)
ccLastUpdated = Lens.lens (lastUpdated :: Crawler -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: Crawler)
{-# DEPRECATED ccLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | For scheduled crawlers, the schedule when the crawler runs.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSchedule :: Lens.Lens' Crawler (Lude.Maybe Schedule)
ccSchedule = Lens.lens (schedule :: Crawler -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: Crawler)
{-# DEPRECATED ccSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The status of the last crawl, and potentially error information if an error occurred.
--
-- /Note:/ Consider using 'lastCrawl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLastCrawl :: Lens.Lens' Crawler (Lude.Maybe LastCrawlInfo)
ccLastCrawl = Lens.lens (lastCrawl :: Crawler -> Lude.Maybe LastCrawlInfo) (\s a -> s {lastCrawl = a} :: Crawler)
{-# DEPRECATED ccLastCrawl "Use generic-lens or generic-optics with 'lastCrawl' instead." #-}

-- | If the crawler is running, contains the total time elapsed since the last crawl began.
--
-- /Note:/ Consider using 'crawlElapsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCrawlElapsedTime :: Lens.Lens' Crawler (Lude.Maybe Lude.Integer)
ccCrawlElapsedTime = Lens.lens (crawlElapsedTime :: Crawler -> Lude.Maybe Lude.Integer) (\s a -> s {crawlElapsedTime = a} :: Crawler)
{-# DEPRECATED ccCrawlElapsedTime "Use generic-lens or generic-optics with 'crawlElapsedTime' instead." #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRecrawlPolicy :: Lens.Lens' Crawler (Lude.Maybe RecrawlPolicy)
ccRecrawlPolicy = Lens.lens (recrawlPolicy :: Crawler -> Lude.Maybe RecrawlPolicy) (\s a -> s {recrawlPolicy = a} :: Crawler)
{-# DEPRECATED ccRecrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead." #-}

-- | A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClassifiers :: Lens.Lens' Crawler (Lude.Maybe [Lude.Text])
ccClassifiers = Lens.lens (classifiers :: Crawler -> Lude.Maybe [Lude.Text]) (\s a -> s {classifiers = a} :: Crawler)
{-# DEPRECATED ccClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRole :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccRole = Lens.lens (role' :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: Crawler)
{-# DEPRECATED ccRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The name of the crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccName = Lens.lens (name :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Crawler)
{-# DEPRECATED ccName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTargets :: Lens.Lens' Crawler (Lude.Maybe CrawlerTargets)
ccTargets = Lens.lens (targets :: Crawler -> Lude.Maybe CrawlerTargets) (\s a -> s {targets = a} :: Crawler)
{-# DEPRECATED ccTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The version of the crawler.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccVersion :: Lens.Lens' Crawler (Lude.Maybe Lude.Integer)
ccVersion = Lens.lens (version :: Crawler -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: Crawler)
{-# DEPRECATED ccVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the database in which the crawler's output is stored.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDatabaseName :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccDatabaseName = Lens.lens (databaseName :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: Crawler)
{-# DEPRECATED ccDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCrawlerSecurityConfiguration :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccCrawlerSecurityConfiguration = Lens.lens (crawlerSecurityConfiguration :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {crawlerSecurityConfiguration = a} :: Crawler)
{-# DEPRECATED ccCrawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead." #-}

-- | A configuration that specifies whether data lineage is enabled for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLineageConfiguration :: Lens.Lens' Crawler (Lude.Maybe LineageConfiguration)
ccLineageConfiguration = Lens.lens (lineageConfiguration :: Crawler -> Lude.Maybe LineageConfiguration) (\s a -> s {lineageConfiguration = a} :: Crawler)
{-# DEPRECATED ccLineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead." #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConfiguration :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccConfiguration = Lens.lens (configuration :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: Crawler)
{-# DEPRECATED ccConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The prefix added to the names of tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTablePrefix :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccTablePrefix = Lens.lens (tablePrefix :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {tablePrefix = a} :: Crawler)
{-# DEPRECATED ccTablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead." #-}

-- | A description of the crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
ccDescription = Lens.lens (description :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Crawler)
{-# DEPRECATED ccDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Crawler where
  parseJSON =
    Lude.withObject
      "Crawler"
      ( \x ->
          Crawler'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "SchemaChangePolicy")
            Lude.<*> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "LastCrawl")
            Lude.<*> (x Lude..:? "CrawlElapsedTime")
            Lude.<*> (x Lude..:? "RecrawlPolicy")
            Lude.<*> (x Lude..:? "Classifiers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Targets")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "CrawlerSecurityConfiguration")
            Lude.<*> (x Lude..:? "LineageConfiguration")
            Lude.<*> (x Lude..:? "Configuration")
            Lude.<*> (x Lude..:? "TablePrefix")
            Lude.<*> (x Lude..:? "Description")
      )
