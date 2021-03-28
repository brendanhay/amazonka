{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new crawler with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in the @s3Targets@ field, the @jdbcTargets@ field, or the @DynamoDBTargets@ field.
module Network.AWS.Glue.CreateCrawler
    (
    -- * Creating a request
      CreateCrawler (..)
    , mkCreateCrawler
    -- ** Request lenses
    , ccfName
    , ccfRole
    , ccfTargets
    , ccfClassifiers
    , ccfConfiguration
    , ccfCrawlerSecurityConfiguration
    , ccfDatabaseName
    , ccfDescription
    , ccfLineageConfiguration
    , ccfRecrawlPolicy
    , ccfSchedule
    , ccfSchemaChangePolicy
    , ccfTablePrefix
    , ccfTags

    -- * Destructuring the response
    , CreateCrawlerResponse (..)
    , mkCreateCrawlerResponse
    -- ** Response lenses
    , ccrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
  { name :: Types.Name
    -- ^ Name of the new crawler.
  , role' :: Types.Role
    -- ^ The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
  , targets :: Types.CrawlerTargets
    -- ^ A list of collection of targets to crawl.
  , classifiers :: Core.Maybe [Types.NameString]
    -- ^ A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
  , configuration :: Core.Maybe Types.Configuration
    -- ^ Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
  , crawlerSecurityConfiguration :: Core.Maybe Types.CrawlerSecurityConfiguration
    -- ^ The name of the @SecurityConfiguration@ structure to be used by this crawler.
  , databaseName :: Core.Maybe Types.DatabaseName
    -- ^ The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of the new crawler.
  , lineageConfiguration :: Core.Maybe Types.LineageConfiguration
    -- ^ Specifies data lineage configuration settings for the crawler.
  , recrawlPolicy :: Core.Maybe Types.RecrawlPolicy
    -- ^ A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
  , schedule :: Core.Maybe Types.CronExpression
    -- ^ A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
  , schemaChangePolicy :: Core.Maybe Types.SchemaChangePolicy
    -- ^ The policy for the crawler's update and deletion behavior.
  , tablePrefix :: Core.Maybe Types.TablePrefix
    -- ^ The table prefix used for catalog tables that are created.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCrawler' value with any optional fields omitted.
mkCreateCrawler
    :: Types.Name -- ^ 'name'
    -> Types.Role -- ^ 'role\''
    -> Types.CrawlerTargets -- ^ 'targets'
    -> CreateCrawler
mkCreateCrawler name role' targets
  = CreateCrawler'{name, role', targets, classifiers = Core.Nothing,
                   configuration = Core.Nothing,
                   crawlerSecurityConfiguration = Core.Nothing,
                   databaseName = Core.Nothing, description = Core.Nothing,
                   lineageConfiguration = Core.Nothing, recrawlPolicy = Core.Nothing,
                   schedule = Core.Nothing, schemaChangePolicy = Core.Nothing,
                   tablePrefix = Core.Nothing, tags = Core.Nothing}

-- | Name of the new crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfName :: Lens.Lens' CreateCrawler Types.Name
ccfName = Lens.field @"name"
{-# INLINEABLE ccfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRole :: Lens.Lens' CreateCrawler Types.Role
ccfRole = Lens.field @"role'"
{-# INLINEABLE ccfRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | A list of collection of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTargets :: Lens.Lens' CreateCrawler Types.CrawlerTargets
ccfTargets = Lens.field @"targets"
{-# INLINEABLE ccfTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfClassifiers :: Lens.Lens' CreateCrawler (Core.Maybe [Types.NameString])
ccfClassifiers = Lens.field @"classifiers"
{-# INLINEABLE ccfClassifiers #-}
{-# DEPRECATED classifiers "Use generic-lens or generic-optics with 'classifiers' instead"  #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Types.Configuration)
ccfConfiguration = Lens.field @"configuration"
{-# INLINEABLE ccfConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfCrawlerSecurityConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Types.CrawlerSecurityConfiguration)
ccfCrawlerSecurityConfiguration = Lens.field @"crawlerSecurityConfiguration"
{-# INLINEABLE ccfCrawlerSecurityConfiguration #-}
{-# DEPRECATED crawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead"  #-}

-- | The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDatabaseName :: Lens.Lens' CreateCrawler (Core.Maybe Types.DatabaseName)
ccfDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE ccfDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | A description of the new crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDescription :: Lens.Lens' CreateCrawler (Core.Maybe Types.DescriptionString)
ccfDescription = Lens.field @"description"
{-# INLINEABLE ccfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies data lineage configuration settings for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfLineageConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Types.LineageConfiguration)
ccfLineageConfiguration = Lens.field @"lineageConfiguration"
{-# INLINEABLE ccfLineageConfiguration #-}
{-# DEPRECATED lineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead"  #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRecrawlPolicy :: Lens.Lens' CreateCrawler (Core.Maybe Types.RecrawlPolicy)
ccfRecrawlPolicy = Lens.field @"recrawlPolicy"
{-# INLINEABLE ccfRecrawlPolicy #-}
{-# DEPRECATED recrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead"  #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfSchedule :: Lens.Lens' CreateCrawler (Core.Maybe Types.CronExpression)
ccfSchedule = Lens.field @"schedule"
{-# INLINEABLE ccfSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The policy for the crawler's update and deletion behavior.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfSchemaChangePolicy :: Lens.Lens' CreateCrawler (Core.Maybe Types.SchemaChangePolicy)
ccfSchemaChangePolicy = Lens.field @"schemaChangePolicy"
{-# INLINEABLE ccfSchemaChangePolicy #-}
{-# DEPRECATED schemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead"  #-}

-- | The table prefix used for catalog tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTablePrefix :: Lens.Lens' CreateCrawler (Core.Maybe Types.TablePrefix)
ccfTablePrefix = Lens.field @"tablePrefix"
{-# INLINEABLE ccfTablePrefix #-}
{-# DEPRECATED tablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead"  #-}

-- | The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTags :: Lens.Lens' CreateCrawler (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ccfTags = Lens.field @"tags"
{-# INLINEABLE ccfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCrawler where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCrawler where
        toHeaders CreateCrawler{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.CreateCrawler") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCrawler where
        toJSON CreateCrawler{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name), Core.Just ("Role" Core..= role'),
                  Core.Just ("Targets" Core..= targets),
                  ("Classifiers" Core..=) Core.<$> classifiers,
                  ("Configuration" Core..=) Core.<$> configuration,
                  ("CrawlerSecurityConfiguration" Core..=) Core.<$>
                    crawlerSecurityConfiguration,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("Description" Core..=) Core.<$> description,
                  ("LineageConfiguration" Core..=) Core.<$> lineageConfiguration,
                  ("RecrawlPolicy" Core..=) Core.<$> recrawlPolicy,
                  ("Schedule" Core..=) Core.<$> schedule,
                  ("SchemaChangePolicy" Core..=) Core.<$> schemaChangePolicy,
                  ("TablePrefix" Core..=) Core.<$> tablePrefix,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCrawler where
        type Rs CreateCrawler = CreateCrawlerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateCrawlerResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCrawlerResponse' smart constructor.
newtype CreateCrawlerResponse = CreateCrawlerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCrawlerResponse' value with any optional fields omitted.
mkCreateCrawlerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCrawlerResponse
mkCreateCrawlerResponse responseStatus
  = CreateCrawlerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfrsResponseStatus :: Lens.Lens' CreateCrawlerResponse Core.Int
ccrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
