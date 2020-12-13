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
    cgCreationTime,
    cgState,
    cgSchemaChangePolicy,
    cgLastUpdated,
    cgSchedule,
    cgLastCrawl,
    cgCrawlElapsedTime,
    cgRecrawlPolicy,
    cgClassifiers,
    cgRole,
    cgName,
    cgTargets,
    cgVersion,
    cgDatabaseName,
    cgCrawlerSecurityConfiguration,
    cgLineageConfiguration,
    cgConfiguration,
    cgTablePrefix,
    cgDescription,
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
  { -- | The time that the crawler was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | Indicates whether the crawler is running, or whether a run is pending.
    state :: Lude.Maybe CrawlerState,
    -- | The policy that specifies update and delete behaviors for the crawler.
    schemaChangePolicy :: Lude.Maybe SchemaChangePolicy,
    -- | The time that the crawler was last updated.
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    -- | For scheduled crawlers, the schedule when the crawler runs.
    schedule :: Lude.Maybe Schedule,
    -- | The status of the last crawl, and potentially error information if an error occurred.
    lastCrawl :: Lude.Maybe LastCrawlInfo,
    -- | If the crawler is running, contains the total time elapsed since the last crawl began.
    crawlElapsedTime :: Lude.Maybe Lude.Integer,
    -- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Lude.Maybe RecrawlPolicy,
    -- | A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
    classifiers :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
    role' :: Lude.Maybe Lude.Text,
    -- | The name of the crawler.
    name :: Lude.Maybe Lude.Text,
    -- | A collection of targets to crawl.
    targets :: Lude.Maybe CrawlerTargets,
    -- | The version of the crawler.
    version :: Lude.Maybe Lude.Integer,
    -- | The name of the database in which the crawler's output is stored.
    databaseName :: Lude.Maybe Lude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
    crawlerSecurityConfiguration :: Lude.Maybe Lude.Text,
    -- | A configuration that specifies whether data lineage is enabled for the crawler.
    lineageConfiguration :: Lude.Maybe LineageConfiguration,
    -- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
    configuration :: Lude.Maybe Lude.Text,
    -- | The prefix added to the names of tables that are created.
    tablePrefix :: Lude.Maybe Lude.Text,
    -- | A description of the crawler.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Crawler' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time that the crawler was created.
-- * 'state' - Indicates whether the crawler is running, or whether a run is pending.
-- * 'schemaChangePolicy' - The policy that specifies update and delete behaviors for the crawler.
-- * 'lastUpdated' - The time that the crawler was last updated.
-- * 'schedule' - For scheduled crawlers, the schedule when the crawler runs.
-- * 'lastCrawl' - The status of the last crawl, and potentially error information if an error occurred.
-- * 'crawlElapsedTime' - If the crawler is running, contains the total time elapsed since the last crawl began.
-- * 'recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
-- * 'classifiers' - A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
-- * 'role'' - The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
-- * 'name' - The name of the crawler.
-- * 'targets' - A collection of targets to crawl.
-- * 'version' - The version of the crawler.
-- * 'databaseName' - The name of the database in which the crawler's output is stored.
-- * 'crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
-- * 'lineageConfiguration' - A configuration that specifies whether data lineage is enabled for the crawler.
-- * 'configuration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
-- * 'tablePrefix' - The prefix added to the names of tables that are created.
-- * 'description' - A description of the crawler.
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
cgCreationTime :: Lens.Lens' Crawler (Lude.Maybe Lude.Timestamp)
cgCreationTime = Lens.lens (creationTime :: Crawler -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Crawler)
{-# DEPRECATED cgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Indicates whether the crawler is running, or whether a run is pending.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgState :: Lens.Lens' Crawler (Lude.Maybe CrawlerState)
cgState = Lens.lens (state :: Crawler -> Lude.Maybe CrawlerState) (\s a -> s {state = a} :: Crawler)
{-# DEPRECATED cgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The policy that specifies update and delete behaviors for the crawler.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgSchemaChangePolicy :: Lens.Lens' Crawler (Lude.Maybe SchemaChangePolicy)
cgSchemaChangePolicy = Lens.lens (schemaChangePolicy :: Crawler -> Lude.Maybe SchemaChangePolicy) (\s a -> s {schemaChangePolicy = a} :: Crawler)
{-# DEPRECATED cgSchemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead." #-}

-- | The time that the crawler was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgLastUpdated :: Lens.Lens' Crawler (Lude.Maybe Lude.Timestamp)
cgLastUpdated = Lens.lens (lastUpdated :: Crawler -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: Crawler)
{-# DEPRECATED cgLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | For scheduled crawlers, the schedule when the crawler runs.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgSchedule :: Lens.Lens' Crawler (Lude.Maybe Schedule)
cgSchedule = Lens.lens (schedule :: Crawler -> Lude.Maybe Schedule) (\s a -> s {schedule = a} :: Crawler)
{-# DEPRECATED cgSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The status of the last crawl, and potentially error information if an error occurred.
--
-- /Note:/ Consider using 'lastCrawl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgLastCrawl :: Lens.Lens' Crawler (Lude.Maybe LastCrawlInfo)
cgLastCrawl = Lens.lens (lastCrawl :: Crawler -> Lude.Maybe LastCrawlInfo) (\s a -> s {lastCrawl = a} :: Crawler)
{-# DEPRECATED cgLastCrawl "Use generic-lens or generic-optics with 'lastCrawl' instead." #-}

-- | If the crawler is running, contains the total time elapsed since the last crawl began.
--
-- /Note:/ Consider using 'crawlElapsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCrawlElapsedTime :: Lens.Lens' Crawler (Lude.Maybe Lude.Integer)
cgCrawlElapsedTime = Lens.lens (crawlElapsedTime :: Crawler -> Lude.Maybe Lude.Integer) (\s a -> s {crawlElapsedTime = a} :: Crawler)
{-# DEPRECATED cgCrawlElapsedTime "Use generic-lens or generic-optics with 'crawlElapsedTime' instead." #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRecrawlPolicy :: Lens.Lens' Crawler (Lude.Maybe RecrawlPolicy)
cgRecrawlPolicy = Lens.lens (recrawlPolicy :: Crawler -> Lude.Maybe RecrawlPolicy) (\s a -> s {recrawlPolicy = a} :: Crawler)
{-# DEPRECATED cgRecrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead." #-}

-- | A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgClassifiers :: Lens.Lens' Crawler (Lude.Maybe [Lude.Text])
cgClassifiers = Lens.lens (classifiers :: Crawler -> Lude.Maybe [Lude.Text]) (\s a -> s {classifiers = a} :: Crawler)
{-# DEPRECATED cgClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRole :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgRole = Lens.lens (role' :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: Crawler)
{-# DEPRECATED cgRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The name of the crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgName = Lens.lens (name :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Crawler)
{-# DEPRECATED cgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTargets :: Lens.Lens' Crawler (Lude.Maybe CrawlerTargets)
cgTargets = Lens.lens (targets :: Crawler -> Lude.Maybe CrawlerTargets) (\s a -> s {targets = a} :: Crawler)
{-# DEPRECATED cgTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The version of the crawler.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgVersion :: Lens.Lens' Crawler (Lude.Maybe Lude.Integer)
cgVersion = Lens.lens (version :: Crawler -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: Crawler)
{-# DEPRECATED cgVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the database in which the crawler's output is stored.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDatabaseName :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgDatabaseName = Lens.lens (databaseName :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: Crawler)
{-# DEPRECATED cgDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCrawlerSecurityConfiguration :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgCrawlerSecurityConfiguration = Lens.lens (crawlerSecurityConfiguration :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {crawlerSecurityConfiguration = a} :: Crawler)
{-# DEPRECATED cgCrawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead." #-}

-- | A configuration that specifies whether data lineage is enabled for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgLineageConfiguration :: Lens.Lens' Crawler (Lude.Maybe LineageConfiguration)
cgLineageConfiguration = Lens.lens (lineageConfiguration :: Crawler -> Lude.Maybe LineageConfiguration) (\s a -> s {lineageConfiguration = a} :: Crawler)
{-# DEPRECATED cgLineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead." #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgConfiguration :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgConfiguration = Lens.lens (configuration :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: Crawler)
{-# DEPRECATED cgConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The prefix added to the names of tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTablePrefix :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgTablePrefix = Lens.lens (tablePrefix :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {tablePrefix = a} :: Crawler)
{-# DEPRECATED cgTablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead." #-}

-- | A description of the crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDescription :: Lens.Lens' Crawler (Lude.Maybe Lude.Text)
cgDescription = Lens.lens (description :: Crawler -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Crawler)
{-# DEPRECATED cgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
