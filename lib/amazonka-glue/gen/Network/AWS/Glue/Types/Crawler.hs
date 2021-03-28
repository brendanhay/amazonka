{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Crawler
  ( Crawler (..)
  -- * Smart constructor
  , mkCrawler
  -- * Lenses
  , cgClassifiers
  , cgConfiguration
  , cgCrawlElapsedTime
  , cgCrawlerSecurityConfiguration
  , cgCreationTime
  , cgDatabaseName
  , cgDescription
  , cgLastCrawl
  , cgLastUpdated
  , cgLineageConfiguration
  , cgName
  , cgRecrawlPolicy
  , cgRole
  , cgSchedule
  , cgSchemaChangePolicy
  , cgState
  , cgTablePrefix
  , cgTargets
  , cgVersion
  ) where

import qualified Network.AWS.Glue.Types.Configuration as Types
import qualified Network.AWS.Glue.Types.CrawlerSecurityConfiguration as Types
import qualified Network.AWS.Glue.Types.CrawlerState as Types
import qualified Network.AWS.Glue.Types.CrawlerTargets as Types
import qualified Network.AWS.Glue.Types.DatabaseName as Types
import qualified Network.AWS.Glue.Types.Description as Types
import qualified Network.AWS.Glue.Types.LastCrawlInfo as Types
import qualified Network.AWS.Glue.Types.LineageConfiguration as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.RecrawlPolicy as Types
import qualified Network.AWS.Glue.Types.Role as Types
import qualified Network.AWS.Glue.Types.Schedule as Types
import qualified Network.AWS.Glue.Types.SchemaChangePolicy as Types
import qualified Network.AWS.Glue.Types.TablePrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.
--
-- /See:/ 'mkCrawler' smart constructor.
data Crawler = Crawler'
  { classifiers :: Core.Maybe [Types.NameString]
    -- ^ A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
  , configuration :: Core.Maybe Types.Configuration
    -- ^ Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
  , crawlElapsedTime :: Core.Maybe Core.Integer
    -- ^ If the crawler is running, contains the total time elapsed since the last crawl began.
  , crawlerSecurityConfiguration :: Core.Maybe Types.CrawlerSecurityConfiguration
    -- ^ The name of the @SecurityConfiguration@ structure to be used by this crawler.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the crawler was created.
  , databaseName :: Core.Maybe Types.DatabaseName
    -- ^ The name of the database in which the crawler's output is stored.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the crawler.
  , lastCrawl :: Core.Maybe Types.LastCrawlInfo
    -- ^ The status of the last crawl, and potentially error information if an error occurred.
  , lastUpdated :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the crawler was last updated.
  , lineageConfiguration :: Core.Maybe Types.LineageConfiguration
    -- ^ A configuration that specifies whether data lineage is enabled for the crawler.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the crawler.
  , recrawlPolicy :: Core.Maybe Types.RecrawlPolicy
    -- ^ A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
  , role' :: Core.Maybe Types.Role
    -- ^ The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
  , schedule :: Core.Maybe Types.Schedule
    -- ^ For scheduled crawlers, the schedule when the crawler runs.
  , schemaChangePolicy :: Core.Maybe Types.SchemaChangePolicy
    -- ^ The policy that specifies update and delete behaviors for the crawler.
  , state :: Core.Maybe Types.CrawlerState
    -- ^ Indicates whether the crawler is running, or whether a run is pending.
  , tablePrefix :: Core.Maybe Types.TablePrefix
    -- ^ The prefix added to the names of tables that are created.
  , targets :: Core.Maybe Types.CrawlerTargets
    -- ^ A collection of targets to crawl.
  , version :: Core.Maybe Core.Integer
    -- ^ The version of the crawler.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Crawler' value with any optional fields omitted.
mkCrawler
    :: Crawler
mkCrawler
  = Crawler'{classifiers = Core.Nothing,
             configuration = Core.Nothing, crawlElapsedTime = Core.Nothing,
             crawlerSecurityConfiguration = Core.Nothing,
             creationTime = Core.Nothing, databaseName = Core.Nothing,
             description = Core.Nothing, lastCrawl = Core.Nothing,
             lastUpdated = Core.Nothing, lineageConfiguration = Core.Nothing,
             name = Core.Nothing, recrawlPolicy = Core.Nothing,
             role' = Core.Nothing, schedule = Core.Nothing,
             schemaChangePolicy = Core.Nothing, state = Core.Nothing,
             tablePrefix = Core.Nothing, targets = Core.Nothing,
             version = Core.Nothing}

-- | A list of UTF-8 strings that specify the custom classifiers that are associated with the crawler.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgClassifiers :: Lens.Lens' Crawler (Core.Maybe [Types.NameString])
cgClassifiers = Lens.field @"classifiers"
{-# INLINEABLE cgClassifiers #-}
{-# DEPRECATED classifiers "Use generic-lens or generic-optics with 'classifiers' instead"  #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgConfiguration :: Lens.Lens' Crawler (Core.Maybe Types.Configuration)
cgConfiguration = Lens.field @"configuration"
{-# INLINEABLE cgConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | If the crawler is running, contains the total time elapsed since the last crawl began.
--
-- /Note:/ Consider using 'crawlElapsedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCrawlElapsedTime :: Lens.Lens' Crawler (Core.Maybe Core.Integer)
cgCrawlElapsedTime = Lens.field @"crawlElapsedTime"
{-# INLINEABLE cgCrawlElapsedTime #-}
{-# DEPRECATED crawlElapsedTime "Use generic-lens or generic-optics with 'crawlElapsedTime' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCrawlerSecurityConfiguration :: Lens.Lens' Crawler (Core.Maybe Types.CrawlerSecurityConfiguration)
cgCrawlerSecurityConfiguration = Lens.field @"crawlerSecurityConfiguration"
{-# INLINEABLE cgCrawlerSecurityConfiguration #-}
{-# DEPRECATED crawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead"  #-}

-- | The time that the crawler was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCreationTime :: Lens.Lens' Crawler (Core.Maybe Core.NominalDiffTime)
cgCreationTime = Lens.field @"creationTime"
{-# INLINEABLE cgCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the database in which the crawler's output is stored.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDatabaseName :: Lens.Lens' Crawler (Core.Maybe Types.DatabaseName)
cgDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE cgDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | A description of the crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDescription :: Lens.Lens' Crawler (Core.Maybe Types.Description)
cgDescription = Lens.field @"description"
{-# INLINEABLE cgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The status of the last crawl, and potentially error information if an error occurred.
--
-- /Note:/ Consider using 'lastCrawl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgLastCrawl :: Lens.Lens' Crawler (Core.Maybe Types.LastCrawlInfo)
cgLastCrawl = Lens.field @"lastCrawl"
{-# INLINEABLE cgLastCrawl #-}
{-# DEPRECATED lastCrawl "Use generic-lens or generic-optics with 'lastCrawl' instead"  #-}

-- | The time that the crawler was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgLastUpdated :: Lens.Lens' Crawler (Core.Maybe Core.NominalDiffTime)
cgLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE cgLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | A configuration that specifies whether data lineage is enabled for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgLineageConfiguration :: Lens.Lens' Crawler (Core.Maybe Types.LineageConfiguration)
cgLineageConfiguration = Lens.field @"lineageConfiguration"
{-# INLINEABLE cgLineageConfiguration #-}
{-# DEPRECATED lineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead"  #-}

-- | The name of the crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgName :: Lens.Lens' Crawler (Core.Maybe Types.Name)
cgName = Lens.field @"name"
{-# INLINEABLE cgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRecrawlPolicy :: Lens.Lens' Crawler (Core.Maybe Types.RecrawlPolicy)
cgRecrawlPolicy = Lens.field @"recrawlPolicy"
{-# INLINEABLE cgRecrawlPolicy #-}
{-# DEPRECATED recrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead"  #-}

-- | The Amazon Resource Name (ARN) of an IAM role that's used to access customer resources, such as Amazon Simple Storage Service (Amazon S3) data.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRole :: Lens.Lens' Crawler (Core.Maybe Types.Role)
cgRole = Lens.field @"role'"
{-# INLINEABLE cgRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | For scheduled crawlers, the schedule when the crawler runs.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgSchedule :: Lens.Lens' Crawler (Core.Maybe Types.Schedule)
cgSchedule = Lens.field @"schedule"
{-# INLINEABLE cgSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The policy that specifies update and delete behaviors for the crawler.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgSchemaChangePolicy :: Lens.Lens' Crawler (Core.Maybe Types.SchemaChangePolicy)
cgSchemaChangePolicy = Lens.field @"schemaChangePolicy"
{-# INLINEABLE cgSchemaChangePolicy #-}
{-# DEPRECATED schemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead"  #-}

-- | Indicates whether the crawler is running, or whether a run is pending.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgState :: Lens.Lens' Crawler (Core.Maybe Types.CrawlerState)
cgState = Lens.field @"state"
{-# INLINEABLE cgState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The prefix added to the names of tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTablePrefix :: Lens.Lens' Crawler (Core.Maybe Types.TablePrefix)
cgTablePrefix = Lens.field @"tablePrefix"
{-# INLINEABLE cgTablePrefix #-}
{-# DEPRECATED tablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead"  #-}

-- | A collection of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTargets :: Lens.Lens' Crawler (Core.Maybe Types.CrawlerTargets)
cgTargets = Lens.field @"targets"
{-# INLINEABLE cgTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The version of the crawler.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgVersion :: Lens.Lens' Crawler (Core.Maybe Core.Integer)
cgVersion = Lens.field @"version"
{-# INLINEABLE cgVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON Crawler where
        parseJSON
          = Core.withObject "Crawler" Core.$
              \ x ->
                Crawler' Core.<$>
                  (x Core..:? "Classifiers") Core.<*> x Core..:? "Configuration"
                    Core.<*> x Core..:? "CrawlElapsedTime"
                    Core.<*> x Core..:? "CrawlerSecurityConfiguration"
                    Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "LastCrawl"
                    Core.<*> x Core..:? "LastUpdated"
                    Core.<*> x Core..:? "LineageConfiguration"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "RecrawlPolicy"
                    Core.<*> x Core..:? "Role"
                    Core.<*> x Core..:? "Schedule"
                    Core.<*> x Core..:? "SchemaChangePolicy"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "TablePrefix"
                    Core.<*> x Core..:? "Targets"
                    Core.<*> x Core..:? "Version"
