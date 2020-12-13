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
    ccfSchemaChangePolicy,
    ccfSchedule,
    ccfRecrawlPolicy,
    ccfClassifiers,
    ccfRole,
    ccfName,
    ccfTargets,
    ccfDatabaseName,
    ccfCrawlerSecurityConfiguration,
    ccfLineageConfiguration,
    ccfConfiguration,
    ccfTablePrefix,
    ccfDescription,
    ccfTags,

    -- * Destructuring the response
    CreateCrawlerResponse (..),
    mkCreateCrawlerResponse,

    -- ** Response lenses
    ccfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
  { -- | The policy for the crawler's update and deletion behavior.
    schemaChangePolicy :: Lude.Maybe SchemaChangePolicy,
    -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Lude.Maybe Lude.Text,
    -- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Lude.Maybe RecrawlPolicy,
    -- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
    classifiers :: Lude.Maybe [Lude.Text],
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
    role' :: Lude.Text,
    -- | Name of the new crawler.
    name :: Lude.Text,
    -- | A list of collection of targets to crawl.
    targets :: CrawlerTargets,
    -- | The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
    databaseName :: Lude.Maybe Lude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
    crawlerSecurityConfiguration :: Lude.Maybe Lude.Text,
    -- | Specifies data lineage configuration settings for the crawler.
    lineageConfiguration :: Lude.Maybe LineageConfiguration,
    -- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
    configuration :: Lude.Maybe Lude.Text,
    -- | The table prefix used for catalog tables that are created.
    tablePrefix :: Lude.Maybe Lude.Text,
    -- | A description of the new crawler.
    description :: Lude.Maybe Lude.Text,
    -- | The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCrawler' with the minimum fields required to make a request.
--
-- * 'schemaChangePolicy' - The policy for the crawler's update and deletion behavior.
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
-- * 'classifiers' - A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
-- * 'role'' - The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
-- * 'name' - Name of the new crawler.
-- * 'targets' - A list of collection of targets to crawl.
-- * 'databaseName' - The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
-- * 'crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
-- * 'lineageConfiguration' - Specifies data lineage configuration settings for the crawler.
-- * 'configuration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
-- * 'tablePrefix' - The table prefix used for catalog tables that are created.
-- * 'description' - A description of the new crawler.
-- * 'tags' - The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
mkCreateCrawler ::
  -- | 'role''
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'targets'
  CrawlerTargets ->
  CreateCrawler
mkCreateCrawler pRole_ pName_ pTargets_ =
  CreateCrawler'
    { schemaChangePolicy = Lude.Nothing,
      schedule = Lude.Nothing,
      recrawlPolicy = Lude.Nothing,
      classifiers = Lude.Nothing,
      role' = pRole_,
      name = pName_,
      targets = pTargets_,
      databaseName = Lude.Nothing,
      crawlerSecurityConfiguration = Lude.Nothing,
      lineageConfiguration = Lude.Nothing,
      configuration = Lude.Nothing,
      tablePrefix = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The policy for the crawler's update and deletion behavior.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfSchemaChangePolicy :: Lens.Lens' CreateCrawler (Lude.Maybe SchemaChangePolicy)
ccfSchemaChangePolicy = Lens.lens (schemaChangePolicy :: CreateCrawler -> Lude.Maybe SchemaChangePolicy) (\s a -> s {schemaChangePolicy = a} :: CreateCrawler)
{-# DEPRECATED ccfSchemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfSchedule :: Lens.Lens' CreateCrawler (Lude.Maybe Lude.Text)
ccfSchedule = Lens.lens (schedule :: CreateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: CreateCrawler)
{-# DEPRECATED ccfSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRecrawlPolicy :: Lens.Lens' CreateCrawler (Lude.Maybe RecrawlPolicy)
ccfRecrawlPolicy = Lens.lens (recrawlPolicy :: CreateCrawler -> Lude.Maybe RecrawlPolicy) (\s a -> s {recrawlPolicy = a} :: CreateCrawler)
{-# DEPRECATED ccfRecrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead." #-}

-- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfClassifiers :: Lens.Lens' CreateCrawler (Lude.Maybe [Lude.Text])
ccfClassifiers = Lens.lens (classifiers :: CreateCrawler -> Lude.Maybe [Lude.Text]) (\s a -> s {classifiers = a} :: CreateCrawler)
{-# DEPRECATED ccfClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the new crawler to access customer resources.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRole :: Lens.Lens' CreateCrawler Lude.Text
ccfRole = Lens.lens (role' :: CreateCrawler -> Lude.Text) (\s a -> s {role' = a} :: CreateCrawler)
{-# DEPRECATED ccfRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | Name of the new crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfName :: Lens.Lens' CreateCrawler Lude.Text
ccfName = Lens.lens (name :: CreateCrawler -> Lude.Text) (\s a -> s {name = a} :: CreateCrawler)
{-# DEPRECATED ccfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of collection of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTargets :: Lens.Lens' CreateCrawler CrawlerTargets
ccfTargets = Lens.lens (targets :: CreateCrawler -> CrawlerTargets) (\s a -> s {targets = a} :: CreateCrawler)
{-# DEPRECATED ccfTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The AWS Glue database where results are written, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDatabaseName :: Lens.Lens' CreateCrawler (Lude.Maybe Lude.Text)
ccfDatabaseName = Lens.lens (databaseName :: CreateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: CreateCrawler)
{-# DEPRECATED ccfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfCrawlerSecurityConfiguration :: Lens.Lens' CreateCrawler (Lude.Maybe Lude.Text)
ccfCrawlerSecurityConfiguration = Lens.lens (crawlerSecurityConfiguration :: CreateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {crawlerSecurityConfiguration = a} :: CreateCrawler)
{-# DEPRECATED ccfCrawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead." #-}

-- | Specifies data lineage configuration settings for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfLineageConfiguration :: Lens.Lens' CreateCrawler (Lude.Maybe LineageConfiguration)
ccfLineageConfiguration = Lens.lens (lineageConfiguration :: CreateCrawler -> Lude.Maybe LineageConfiguration) (\s a -> s {lineageConfiguration = a} :: CreateCrawler)
{-# DEPRECATED ccfLineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead." #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfConfiguration :: Lens.Lens' CreateCrawler (Lude.Maybe Lude.Text)
ccfConfiguration = Lens.lens (configuration :: CreateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: CreateCrawler)
{-# DEPRECATED ccfConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The table prefix used for catalog tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTablePrefix :: Lens.Lens' CreateCrawler (Lude.Maybe Lude.Text)
ccfTablePrefix = Lens.lens (tablePrefix :: CreateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {tablePrefix = a} :: CreateCrawler)
{-# DEPRECATED ccfTablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead." #-}

-- | A description of the new crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDescription :: Lens.Lens' CreateCrawler (Lude.Maybe Lude.Text)
ccfDescription = Lens.lens (description :: CreateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateCrawler)
{-# DEPRECATED ccfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to use with this crawler request. You may use tags to limit access to the crawler. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTags :: Lens.Lens' CreateCrawler (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccfTags = Lens.lens (tags :: CreateCrawler -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateCrawler)
{-# DEPRECATED ccfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateCrawler where
  type Rs CreateCrawler = CreateCrawlerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateCrawlerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCrawler where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateCrawler" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCrawler where
  toJSON CreateCrawler' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaChangePolicy" Lude..=) Lude.<$> schemaChangePolicy,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("RecrawlPolicy" Lude..=) Lude.<$> recrawlPolicy,
            ("Classifiers" Lude..=) Lude.<$> classifiers,
            Lude.Just ("Role" Lude..= role'),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Targets" Lude..= targets),
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("CrawlerSecurityConfiguration" Lude..=)
              Lude.<$> crawlerSecurityConfiguration,
            ("LineageConfiguration" Lude..=) Lude.<$> lineageConfiguration,
            ("Configuration" Lude..=) Lude.<$> configuration,
            ("TablePrefix" Lude..=) Lude.<$> tablePrefix,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCrawlerResponse' smart constructor.
newtype CreateCrawlerResponse = CreateCrawlerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCrawlerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateCrawlerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCrawlerResponse
mkCreateCrawlerResponse pResponseStatus_ =
  CreateCrawlerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrsResponseStatus :: Lens.Lens' CreateCrawlerResponse Lude.Int
ccfrsResponseStatus = Lens.lens (responseStatus :: CreateCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCrawlerResponse)
{-# DEPRECATED ccfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
