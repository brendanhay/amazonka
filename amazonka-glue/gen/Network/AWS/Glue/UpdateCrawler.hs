{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateCrawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a crawler. If a crawler is running, you must stop it using
-- @StopCrawler@ before updating it.
module Network.AWS.Glue.UpdateCrawler
  ( -- * Creating a Request
    UpdateCrawler (..),
    newUpdateCrawler,

    -- * Request Lenses
    updateCrawler_schemaChangePolicy,
    updateCrawler_recrawlPolicy,
    updateCrawler_classifiers,
    updateCrawler_configuration,
    updateCrawler_lineageConfiguration,
    updateCrawler_targets,
    updateCrawler_role,
    updateCrawler_tablePrefix,
    updateCrawler_description,
    updateCrawler_schedule,
    updateCrawler_crawlerSecurityConfiguration,
    updateCrawler_databaseName,
    updateCrawler_name,

    -- * Destructuring the Response
    UpdateCrawlerResponse (..),
    newUpdateCrawlerResponse,

    -- * Response Lenses
    updateCrawlerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCrawler' smart constructor.
data UpdateCrawler = UpdateCrawler'
  { -- | The policy for the crawler\'s update and deletion behavior.
    schemaChangePolicy :: Core.Maybe SchemaChangePolicy,
    -- | A policy that specifies whether to crawl the entire dataset again, or to
    -- crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Core.Maybe RecrawlPolicy,
    -- | A list of custom classifiers that the user has registered. By default,
    -- all built-in classifiers are included in a crawl, but these custom
    -- classifiers always override the default classifiers for a given
    -- classification.
    classifiers :: Core.Maybe [Core.Text],
    -- | Crawler configuration information. This versioned JSON string allows
    -- users to specify aspects of a crawler\'s behavior. For more information,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
    configuration :: Core.Maybe Core.Text,
    -- | Specifies data lineage configuration settings for the crawler.
    lineageConfiguration :: Core.Maybe LineageConfiguration,
    -- | A list of targets to crawl.
    targets :: Core.Maybe CrawlerTargets,
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used
    -- by the new crawler to access customer resources.
    role' :: Core.Maybe Core.Text,
    -- | The table prefix used for catalog tables that are created.
    tablePrefix :: Core.Maybe Core.Text,
    -- | A description of the new crawler.
    description :: Core.Maybe Core.Text,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Core.Maybe Core.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Core.Maybe Core.Text,
    -- | The AWS Glue database where results are stored, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    databaseName :: Core.Maybe Core.Text,
    -- | Name of the new crawler.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCrawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaChangePolicy', 'updateCrawler_schemaChangePolicy' - The policy for the crawler\'s update and deletion behavior.
--
-- 'recrawlPolicy', 'updateCrawler_recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
--
-- 'classifiers', 'updateCrawler_classifiers' - A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
--
-- 'configuration', 'updateCrawler_configuration' - Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
--
-- 'lineageConfiguration', 'updateCrawler_lineageConfiguration' - Specifies data lineage configuration settings for the crawler.
--
-- 'targets', 'updateCrawler_targets' - A list of targets to crawl.
--
-- 'role'', 'updateCrawler_role' - The IAM role or Amazon Resource Name (ARN) of an IAM role that is used
-- by the new crawler to access customer resources.
--
-- 'tablePrefix', 'updateCrawler_tablePrefix' - The table prefix used for catalog tables that are created.
--
-- 'description', 'updateCrawler_description' - A description of the new crawler.
--
-- 'schedule', 'updateCrawler_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'crawlerSecurityConfiguration', 'updateCrawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'databaseName', 'updateCrawler_databaseName' - The AWS Glue database where results are stored, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
--
-- 'name', 'updateCrawler_name' - Name of the new crawler.
newUpdateCrawler ::
  -- | 'name'
  Core.Text ->
  UpdateCrawler
newUpdateCrawler pName_ =
  UpdateCrawler'
    { schemaChangePolicy = Core.Nothing,
      recrawlPolicy = Core.Nothing,
      classifiers = Core.Nothing,
      configuration = Core.Nothing,
      lineageConfiguration = Core.Nothing,
      targets = Core.Nothing,
      role' = Core.Nothing,
      tablePrefix = Core.Nothing,
      description = Core.Nothing,
      schedule = Core.Nothing,
      crawlerSecurityConfiguration = Core.Nothing,
      databaseName = Core.Nothing,
      name = pName_
    }

-- | The policy for the crawler\'s update and deletion behavior.
updateCrawler_schemaChangePolicy :: Lens.Lens' UpdateCrawler (Core.Maybe SchemaChangePolicy)
updateCrawler_schemaChangePolicy = Lens.lens (\UpdateCrawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@UpdateCrawler' {} a -> s {schemaChangePolicy = a} :: UpdateCrawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
updateCrawler_recrawlPolicy :: Lens.Lens' UpdateCrawler (Core.Maybe RecrawlPolicy)
updateCrawler_recrawlPolicy = Lens.lens (\UpdateCrawler' {recrawlPolicy} -> recrawlPolicy) (\s@UpdateCrawler' {} a -> s {recrawlPolicy = a} :: UpdateCrawler)

-- | A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
updateCrawler_classifiers :: Lens.Lens' UpdateCrawler (Core.Maybe [Core.Text])
updateCrawler_classifiers = Lens.lens (\UpdateCrawler' {classifiers} -> classifiers) (\s@UpdateCrawler' {} a -> s {classifiers = a} :: UpdateCrawler) Core.. Lens.mapping Lens._Coerce

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
updateCrawler_configuration :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_configuration = Lens.lens (\UpdateCrawler' {configuration} -> configuration) (\s@UpdateCrawler' {} a -> s {configuration = a} :: UpdateCrawler)

-- | Specifies data lineage configuration settings for the crawler.
updateCrawler_lineageConfiguration :: Lens.Lens' UpdateCrawler (Core.Maybe LineageConfiguration)
updateCrawler_lineageConfiguration = Lens.lens (\UpdateCrawler' {lineageConfiguration} -> lineageConfiguration) (\s@UpdateCrawler' {} a -> s {lineageConfiguration = a} :: UpdateCrawler)

-- | A list of targets to crawl.
updateCrawler_targets :: Lens.Lens' UpdateCrawler (Core.Maybe CrawlerTargets)
updateCrawler_targets = Lens.lens (\UpdateCrawler' {targets} -> targets) (\s@UpdateCrawler' {} a -> s {targets = a} :: UpdateCrawler)

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used
-- by the new crawler to access customer resources.
updateCrawler_role :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_role = Lens.lens (\UpdateCrawler' {role'} -> role') (\s@UpdateCrawler' {} a -> s {role' = a} :: UpdateCrawler)

-- | The table prefix used for catalog tables that are created.
updateCrawler_tablePrefix :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_tablePrefix = Lens.lens (\UpdateCrawler' {tablePrefix} -> tablePrefix) (\s@UpdateCrawler' {} a -> s {tablePrefix = a} :: UpdateCrawler)

-- | A description of the new crawler.
updateCrawler_description :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_description = Lens.lens (\UpdateCrawler' {description} -> description) (\s@UpdateCrawler' {} a -> s {description = a} :: UpdateCrawler)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
updateCrawler_schedule :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_schedule = Lens.lens (\UpdateCrawler' {schedule} -> schedule) (\s@UpdateCrawler' {} a -> s {schedule = a} :: UpdateCrawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
updateCrawler_crawlerSecurityConfiguration :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_crawlerSecurityConfiguration = Lens.lens (\UpdateCrawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@UpdateCrawler' {} a -> s {crawlerSecurityConfiguration = a} :: UpdateCrawler)

-- | The AWS Glue database where results are stored, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
updateCrawler_databaseName :: Lens.Lens' UpdateCrawler (Core.Maybe Core.Text)
updateCrawler_databaseName = Lens.lens (\UpdateCrawler' {databaseName} -> databaseName) (\s@UpdateCrawler' {} a -> s {databaseName = a} :: UpdateCrawler)

-- | Name of the new crawler.
updateCrawler_name :: Lens.Lens' UpdateCrawler Core.Text
updateCrawler_name = Lens.lens (\UpdateCrawler' {name} -> name) (\s@UpdateCrawler' {} a -> s {name = a} :: UpdateCrawler)

instance Core.AWSRequest UpdateCrawler where
  type
    AWSResponse UpdateCrawler =
      UpdateCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCrawlerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCrawler

instance Core.NFData UpdateCrawler

instance Core.ToHeaders UpdateCrawler where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateCrawler" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCrawler where
  toJSON UpdateCrawler' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaChangePolicy" Core..=)
              Core.<$> schemaChangePolicy,
            ("RecrawlPolicy" Core..=) Core.<$> recrawlPolicy,
            ("Classifiers" Core..=) Core.<$> classifiers,
            ("Configuration" Core..=) Core.<$> configuration,
            ("LineageConfiguration" Core..=)
              Core.<$> lineageConfiguration,
            ("Targets" Core..=) Core.<$> targets,
            ("Role" Core..=) Core.<$> role',
            ("TablePrefix" Core..=) Core.<$> tablePrefix,
            ("Description" Core..=) Core.<$> description,
            ("Schedule" Core..=) Core.<$> schedule,
            ("CrawlerSecurityConfiguration" Core..=)
              Core.<$> crawlerSecurityConfiguration,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateCrawler where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCrawler where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCrawlerResponse' smart constructor.
data UpdateCrawlerResponse = UpdateCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCrawlerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCrawlerResponse_httpStatus' - The response's http status code.
newUpdateCrawlerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateCrawlerResponse
newUpdateCrawlerResponse pHttpStatus_ =
  UpdateCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateCrawlerResponse_httpStatus :: Lens.Lens' UpdateCrawlerResponse Core.Int
updateCrawlerResponse_httpStatus = Lens.lens (\UpdateCrawlerResponse' {httpStatus} -> httpStatus) (\s@UpdateCrawlerResponse' {} a -> s {httpStatus = a} :: UpdateCrawlerResponse)

instance Core.NFData UpdateCrawlerResponse
