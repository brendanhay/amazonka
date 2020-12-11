{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    uSchemaChangePolicy,
    uSchedule,
    uRecrawlPolicy,
    uClassifiers,
    uRole,
    uTargets,
    uDatabaseName,
    uCrawlerSecurityConfiguration,
    uLineageConfiguration,
    uConfiguration,
    uTablePrefix,
    uDescription,
    uName,

    -- * Destructuring the response
    UpdateCrawlerResponse (..),
    mkUpdateCrawlerResponse,

    -- ** Response lenses
    uccrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCrawler' smart constructor.
data UpdateCrawler = UpdateCrawler'
  { schemaChangePolicy ::
      Lude.Maybe SchemaChangePolicy,
    schedule :: Lude.Maybe Lude.Text,
    recrawlPolicy :: Lude.Maybe RecrawlPolicy,
    classifiers :: Lude.Maybe [Lude.Text],
    role' :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe CrawlerTargets,
    databaseName :: Lude.Maybe Lude.Text,
    crawlerSecurityConfiguration :: Lude.Maybe Lude.Text,
    lineageConfiguration :: Lude.Maybe LineageConfiguration,
    configuration :: Lude.Maybe Lude.Text,
    tablePrefix :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCrawler' with the minimum fields required to make a request.
--
-- * 'classifiers' - A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
-- * 'configuration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
-- * 'crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this crawler.
-- * 'databaseName' - The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
-- * 'description' - A description of the new crawler.
-- * 'lineageConfiguration' - Specifies data lineage configuration settings for the crawler.
-- * 'name' - Name of the new crawler.
-- * 'recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
-- * 'role'' - The IAM role or Amazon Resource Name (ARN) of an IAM role that is used by the new crawler to access customer resources.
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
-- * 'schemaChangePolicy' - The policy for the crawler's update and deletion behavior.
-- * 'tablePrefix' - The table prefix used for catalog tables that are created.
-- * 'targets' - A list of targets to crawl.
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
      targets = Lude.Nothing,
      databaseName = Lude.Nothing,
      crawlerSecurityConfiguration = Lude.Nothing,
      lineageConfiguration = Lude.Nothing,
      configuration = Lude.Nothing,
      tablePrefix = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_
    }

-- | The policy for the crawler's update and deletion behavior.
--
-- /Note:/ Consider using 'schemaChangePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSchemaChangePolicy :: Lens.Lens' UpdateCrawler (Lude.Maybe SchemaChangePolicy)
uSchemaChangePolicy = Lens.lens (schemaChangePolicy :: UpdateCrawler -> Lude.Maybe SchemaChangePolicy) (\s a -> s {schemaChangePolicy = a} :: UpdateCrawler)
{-# DEPRECATED uSchemaChangePolicy "Use generic-lens or generic-optics with 'schemaChangePolicy' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSchedule :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uSchedule = Lens.lens (schedule :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: UpdateCrawler)
{-# DEPRECATED uSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | A policy that specifies whether to crawl the entire dataset again, or to crawl only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRecrawlPolicy :: Lens.Lens' UpdateCrawler (Lude.Maybe RecrawlPolicy)
uRecrawlPolicy = Lens.lens (recrawlPolicy :: UpdateCrawler -> Lude.Maybe RecrawlPolicy) (\s a -> s {recrawlPolicy = a} :: UpdateCrawler)
{-# DEPRECATED uRecrawlPolicy "Use generic-lens or generic-optics with 'recrawlPolicy' instead." #-}

-- | A list of custom classifiers that the user has registered. By default, all built-in classifiers are included in a crawl, but these custom classifiers always override the default classifiers for a given classification.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uClassifiers :: Lens.Lens' UpdateCrawler (Lude.Maybe [Lude.Text])
uClassifiers = Lens.lens (classifiers :: UpdateCrawler -> Lude.Maybe [Lude.Text]) (\s a -> s {classifiers = a} :: UpdateCrawler)
{-# DEPRECATED uClassifiers "Use generic-lens or generic-optics with 'classifiers' instead." #-}

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used by the new crawler to access customer resources.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRole :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uRole = Lens.lens (role' :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: UpdateCrawler)
{-# DEPRECATED uRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | A list of targets to crawl.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTargets :: Lens.Lens' UpdateCrawler (Lude.Maybe CrawlerTargets)
uTargets = Lens.lens (targets :: UpdateCrawler -> Lude.Maybe CrawlerTargets) (\s a -> s {targets = a} :: UpdateCrawler)
{-# DEPRECATED uTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The AWS Glue database where results are stored, such as: @arn:aws:daylight:us-east-1::database/sometable/*@ .
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDatabaseName :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uDatabaseName = Lens.lens (databaseName :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: UpdateCrawler)
{-# DEPRECATED uDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the @SecurityConfiguration@ structure to be used by this crawler.
--
-- /Note:/ Consider using 'crawlerSecurityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uCrawlerSecurityConfiguration :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uCrawlerSecurityConfiguration = Lens.lens (crawlerSecurityConfiguration :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {crawlerSecurityConfiguration = a} :: UpdateCrawler)
{-# DEPRECATED uCrawlerSecurityConfiguration "Use generic-lens or generic-optics with 'crawlerSecurityConfiguration' instead." #-}

-- | Specifies data lineage configuration settings for the crawler.
--
-- /Note:/ Consider using 'lineageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLineageConfiguration :: Lens.Lens' UpdateCrawler (Lude.Maybe LineageConfiguration)
uLineageConfiguration = Lens.lens (lineageConfiguration :: UpdateCrawler -> Lude.Maybe LineageConfiguration) (\s a -> s {lineageConfiguration = a} :: UpdateCrawler)
{-# DEPRECATED uLineageConfiguration "Use generic-lens or generic-optics with 'lineageConfiguration' instead." #-}

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a crawler's behavior. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler> .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConfiguration :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uConfiguration = Lens.lens (configuration :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {configuration = a} :: UpdateCrawler)
{-# DEPRECATED uConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The table prefix used for catalog tables that are created.
--
-- /Note:/ Consider using 'tablePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTablePrefix :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uTablePrefix = Lens.lens (tablePrefix :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {tablePrefix = a} :: UpdateCrawler)
{-# DEPRECATED uTablePrefix "Use generic-lens or generic-optics with 'tablePrefix' instead." #-}

-- | A description of the new crawler.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDescription :: Lens.Lens' UpdateCrawler (Lude.Maybe Lude.Text)
uDescription = Lens.lens (description :: UpdateCrawler -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateCrawler)
{-# DEPRECATED uDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Name of the new crawler.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' UpdateCrawler Lude.Text
uName = Lens.lens (name :: UpdateCrawler -> Lude.Text) (\s a -> s {name = a} :: UpdateCrawler)
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

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
            ("Targets" Lude..=) Lude.<$> targets,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("CrawlerSecurityConfiguration" Lude..=)
              Lude.<$> crawlerSecurityConfiguration,
            ("LineageConfiguration" Lude..=) Lude.<$> lineageConfiguration,
            ("Configuration" Lude..=) Lude.<$> configuration,
            ("TablePrefix" Lude..=) Lude.<$> tablePrefix,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateCrawler where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCrawler where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCrawlerResponse' smart constructor.
newtype UpdateCrawlerResponse = UpdateCrawlerResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
uccrsResponseStatus :: Lens.Lens' UpdateCrawlerResponse Lude.Int
uccrsResponseStatus = Lens.lens (responseStatus :: UpdateCrawlerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCrawlerResponse)
{-# DEPRECATED uccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
