{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a crawler. If a crawler is running, you must stop it using @StopCrawler@ before updating it.
module Network.AWS.Glue.UpdateCrawler
  ( -- * Creating a request
    UpdateCrawler (..),
    mkUpdateCrawler,

    -- ** Request lenses
    ucSchemaChangePolicy,
    ucSchedule,
    ucRecrawlPolicy,
    ucClassifiers,
    ucRole,
    ucName,
    ucTargets,
    ucDatabaseName,
    ucCrawlerSecurityConfiguration,
    ucLineageConfiguration,
    ucConfiguration,
    ucTablePrefix,
    ucDescription,

    -- * Destructuring the response
    UpdateCrawlerResponse (..),
    mkUpdateCrawlerResponse,

    -- ** Response lenses
    ucfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCrawler' smart constructor.
data UpdateCrawler = UpdateCrawler'
  { -- | The policy for the crawler's update and deletion behavior.
    schemaChangePolicy :: Lude.Maybe SchemaChangePolicy,
    -- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
    schedule :: Lude.Maybe Lude.Text,
    -- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Lude.Maybe RecrawlPolicy,
    -- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
    classifiers :: Lude.Maybe [Lude.Text],
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used by the new crawler to access customer resources.
    role' :: Lude.Maybe Lude.Text,
    -- | Name of the new crawler.
    name :: Lude.Text,
    -- | A list of targets to crawl.
    targets :: Lude.Maybe CrawlerTargets,
    -- | The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
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
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCrawler' with the minimum fields required to make a request.
--
-- * 'schemaChangePolicy' - The policy for the crawler's update and deletion behavior.
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
-- * 'classifiers' - A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
-- * 'role'' - The IAM role or Amazon Resource Name (ARN) of an IAM role that is used by the new crawler to access customer resources.
-- * 'name' - Name of the new crawler.
-- * 'targets' - A list of targets to crawl.
-- * 'databaseName' - The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
-- * 'crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
-- * 'lineageConfiguration' - Specifies data lineage configuration settings for the crawler.
-- * 'configuration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
-- * 'tablePrefix' - The table prefix used for catalog tables that are created.
-- * 'description' - A description of the new crawler.
mkUpdateCrawler ::
  -- | 'name'
  Lude.Text ->
  UpdateCrawler
mkUpdateCrawler pName_ =
  UpdateCrawler'
    { schemaChangePolicy = Lude.Nothing,
      schedule = Lude.Nothing,
      recrawlPolicy = Lude.Nothing,
      classifiers = Lude.Nothing,
      role' = Lude.Nothing,
      name = pName_,
      targets = Lude.Nothing,
      databaseName = Lude.Nothing,
      crawlerSecurityConfiguration = Lude.Nothing,
      lineageConfiguration = Lude.Nothing,
      configuration = Lude.Nothing,
      tablePrefix = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The policy for the crawler's update and deletion behavior.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucSchemaChangePolicy :: Lens.Lens' UpdateCrawler (Lude.Maybe SchemaChangePolicy)
ucSchemaChangePolicy = Lens.lens (schemaChangePolicy :: UpdateCrawler -> Lude.Maybe SchemaChangePolicy) (\s a -> s {schemaChangePolicy = a} :: UpdateCrawler)
{-# DEPRECATED ucSchemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucSchedule :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucSchedule = Lens.lens (schedule :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: UpdateCrawler)
{-# DEPRECATED ucSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRecrawlPolicy :: Lens.Lens' UpdateCrawler (Lude.Maybe RecrawlPolicy)
ucRecrawlPolicy = Lens.lens (recrawlPolicy :: UpdateCrawler -> Lude.Maybe RecrawlPolicy) (\s a -> s {recrawlPolicy = a} :: UpdateCrawler)
{-# DEPRECATED ucRecrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead." #-}

-- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucClassifiers :: Lens.Lens' UpdateCrawler (Lude.Maybe [Lude.Text])
ucClassifiers = Lens.lens (classifiers :: UpdateCrawler -> Lude.Maybe [Lude.Text]) (\s a -> s {classifiers = a} :: UpdateCrawler)
{-# DEPRECATED ucClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used by the new crawler to access customer resources.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucRole :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucRole = Lens.lens (role' :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: UpdateCrawler)
{-# DEPRECATED ucRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | Name of the new crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucName :: Lens.Lens' UpdateCrawler Lude.Text
ucName = Lens.lens (name :: UpdateCrawler -> Lude.Text) (\s a -> s {name = a} :: UpdateCrawler)
{-# DEPRECATED ucName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucTargets :: Lens.Lens' UpdateCrawler (Lude.Maybe CrawlerTargets)
ucTargets = Lens.lens (targets :: UpdateCrawler -> Lude.Maybe CrawlerTargets) (\s a -> s {targets = a} :: UpdateCrawler)
{-# DEPRECATED ucTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDatabaseName :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucDatabaseName = Lens.lens (databaseName :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: UpdateCrawler)
{-# DEPRECATED ucDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCrawlerSecurityConfiguration :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucCrawlerSecurityConfiguration = Lens.lens (crawlerSecurityConfiguration :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {crawlerSecurityConfiguration = a} :: UpdateCrawler)
{-# DEPRECATED ucCrawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead." #-}

-- | Specifies data lineage configuration settings for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucLineageConfiguration :: Lens.Lens' UpdateCrawler (Lude.Maybe LineageConfiguration)
ucLineageConfiguration = Lens.lens (lineageConfiguration :: UpdateCrawler -> Lude.Maybe LineageConfiguration) (\s a -> s {lineageConfiguration = a} :: UpdateCrawler)
{-# DEPRECATED ucLineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead." #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucConfiguration :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucConfiguration = Lens.lens (configuration :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: UpdateCrawler)
{-# DEPRECATED ucConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The table prefix used for catalog tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucTablePrefix :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucTablePrefix = Lens.lens (tablePrefix :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {tablePrefix = a} :: UpdateCrawler)
{-# DEPRECATED ucTablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead." #-}

-- | A description of the new crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
ucDescription = Lens.lens (description :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateCrawler)
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateCrawler where
  type Rs UpdateCrawler = UpdateCrawlerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateCrawlerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCrawler where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateCrawler" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCrawler where
  toJSON UpdateCrawler' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SchemaChangePolicy" Lude..=) Lude.<$> schemaChangePolicy,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("RecrawlPolicy" Lude..=) Lude.<$> recrawlPolicy,
            ("Classifiers" Lude..=) Lude.<$> classifiers,
            ("Role" Lude..=) Lude.<$> role',
            Lude.Just ("Name" Lude..= name),
            ("Targets" Lude..=) Lude.<$> targets,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("CrawlerSecurityConfiguration" Lude..=)
              Lude.<$> crawlerSecurityConfiguration,
            ("LineageConfiguration" Lude..=) Lude.<$> lineageConfiguration,
            ("Configuration" Lude..=) Lude.<$> configuration,
            ("TablePrefix" Lude..=) Lude.<$> tablePrefix,
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCrawlerResponse' smart constructor.
newtype UpdateCrawlerResponse = UpdateCrawlerResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCrawlerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateCrawlerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCrawlerResponse
mkUpdateCrawlerResponse pResponseStatus_ =
  UpdateCrawlerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfrsResponseStatus :: Lens.Lens' UpdateCrawlerResponse Lude.Int
ucfrsResponseStatus = Lens.lens (responseStatus :: UpdateCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCrawlerResponse)
{-# DEPRECATED ucfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
