{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateCrawler (..),
    mkCreateCrawler,

    -- ** Request lenses
    ccfName,
    ccfRole,
    ccfTargets,
    ccfClassifiers,
    ccfConfiguration,
    ccfCrawlerSecurityConfiguration,
    ccfDatabaseName,
    ccfDescription,
    ccfLineageConfiguration,
    ccfRecrawlPolicy,
    ccfSchedule,
    ccfSchemaChangePolicy,
    ccfTablePrefix,
    ccfTags,

    -- * Destructuring the response
    CreateCrawlerResponse (..),
    mkCreateCrawlerResponse,

    -- ** Response lenses
    ccrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
  { -- | Name of the new crawler.
    name :: Types.Name,
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
    role' :: Types.Role,
    -- | A list of collection of targets to crawl.
    targets :: Types.CrawlerTargets,
    -- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
    classifiers :: Core.Maybe [Types.NameString],
    -- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
    configuration :: Core.Maybe Types.Configuration,
    -- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
    crawlerSecurityConfiguration :: Core.Maybe Types.CrawlerSecurityConfiguration,
    -- | The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
    databaseName :: Core.Maybe Types.DatabaseName,
    -- | A description of the new crawler.
    description :: Core.Maybe Types.DescriptionString,
    -- | Specifies data lineage configuration settings for the crawler.
    lineageConfiguration :: Core.Maybe Types.LineageConfiguration,
    -- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Core.Maybe Types.RecrawlPolicy,
    -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Core.Maybe Types.CronExpression,
    -- | The policy for the crawler's update and deletion behavior.
    schemaChangePolicy :: Core.Maybe Types.SchemaChangePolicy,
    -- | The table prefix used for catalog tables that are created.
    tablePrefix :: Core.Maybe Types.TablePrefix,
    -- | The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCrawler' value with any optional fields omitted.
mkCreateCrawler ::
  -- | 'name'
  Types.Name ->
  -- | 'role\''
  Types.Role ->
  -- | 'targets'
  Types.CrawlerTargets ->
  CreateCrawler
mkCreateCrawler name role' targets =
  CreateCrawler'
    { name,
      role',
      targets,
      classifiers = Core.Nothing,
      configuration = Core.Nothing,
      crawlerSecurityConfiguration = Core.Nothing,
      databaseName = Core.Nothing,
      description = Core.Nothing,
      lineageConfiguration = Core.Nothing,
      recrawlPolicy = Core.Nothing,
      schedule = Core.Nothing,
      schemaChangePolicy = Core.Nothing,
      tablePrefix = Core.Nothing,
      tags = Core.Nothing
    }

-- | Name of the new crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfName :: Lens.Lens' CreateCrawler Types.Name
ccfName = Lens.field @"name"
{-# DEPRECATED ccfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRole :: Lens.Lens' CreateCrawler Types.Role
ccfRole = Lens.field @"role'"
{-# DEPRECATED ccfRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | A list of collection of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTargets :: Lens.Lens' CreateCrawler Types.CrawlerTargets
ccfTargets = Lens.field @"targets"
{-# DEPRECATED ccfTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfClassifiers :: Lens.Lens' CreateCrawler (Core.Maybe [Types.NameString])
ccfClassifiers = Lens.field @"classifiers"
{-# DEPRECATED ccfClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Types.Configuration)
ccfConfiguration = Lens.field @"configuration"
{-# DEPRECATED ccfConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfCrawlerSecurityConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Types.CrawlerSecurityConfiguration)
ccfCrawlerSecurityConfiguration = Lens.field @"crawlerSecurityConfiguration"
{-# DEPRECATED ccfCrawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead." #-}

-- | The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDatabaseName :: Lens.Lens' CreateCrawler (Core.Maybe Types.DatabaseName)
ccfDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED ccfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A description of the new crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDescription :: Lens.Lens' CreateCrawler (Core.Maybe Types.DescriptionString)
ccfDescription = Lens.field @"description"
{-# DEPRECATED ccfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies data lineage configuration settings for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfLineageConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Types.LineageConfiguration)
ccfLineageConfiguration = Lens.field @"lineageConfiguration"
{-# DEPRECATED ccfLineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead." #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRecrawlPolicy :: Lens.Lens' CreateCrawler (Core.Maybe Types.RecrawlPolicy)
ccfRecrawlPolicy = Lens.field @"recrawlPolicy"
{-# DEPRECATED ccfRecrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfSchedule :: Lens.Lens' CreateCrawler (Core.Maybe Types.CronExpression)
ccfSchedule = Lens.field @"schedule"
{-# DEPRECATED ccfSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The policy for the crawler's update and deletion behavior.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfSchemaChangePolicy :: Lens.Lens' CreateCrawler (Core.Maybe Types.SchemaChangePolicy)
ccfSchemaChangePolicy = Lens.field @"schemaChangePolicy"
{-# DEPRECATED ccfSchemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead." #-}

-- | The table prefix used for catalog tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTablePrefix :: Lens.Lens' CreateCrawler (Core.Maybe Types.TablePrefix)
ccfTablePrefix = Lens.field @"tablePrefix"
{-# DEPRECATED ccfTablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead." #-}

-- | The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTags :: Lens.Lens' CreateCrawler (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ccfTags = Lens.field @"tags"
{-# DEPRECATED ccfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateCrawler where
  toJSON CreateCrawler {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Role" Core..= role'),
            Core.Just ("Targets" Core..= targets),
            ("Classifiers" Core..=) Core.<$> classifiers,
            ("Configuration" Core..=) Core.<$> configuration,
            ("CrawlerSecurityConfiguration" Core..=)
              Core.<$> crawlerSecurityConfiguration,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            ("Description" Core..=) Core.<$> description,
            ("LineageConfiguration" Core..=) Core.<$> lineageConfiguration,
            ("RecrawlPolicy" Core..=) Core.<$> recrawlPolicy,
            ("Schedule" Core..=) Core.<$> schedule,
            ("SchemaChangePolicy" Core..=) Core.<$> schemaChangePolicy,
            ("TablePrefix" Core..=) Core.<$> tablePrefix,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateCrawler where
  type Rs CreateCrawler = CreateCrawlerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateCrawler")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCrawlerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCrawlerResponse' smart constructor.
newtype CreateCrawlerResponse = CreateCrawlerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCrawlerResponse' value with any optional fields omitted.
mkCreateCrawlerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCrawlerResponse
mkCreateCrawlerResponse responseStatus =
  CreateCrawlerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrfrsResponseStatus :: Lens.Lens' CreateCrawlerResponse Core.Int
ccrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
