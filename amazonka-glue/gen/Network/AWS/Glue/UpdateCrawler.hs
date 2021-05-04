{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCrawler' smart constructor.
data UpdateCrawler = UpdateCrawler'
  { -- | The policy for the crawler\'s update and deletion behavior.
    schemaChangePolicy :: Prelude.Maybe SchemaChangePolicy,
    -- | A policy that specifies whether to crawl the entire dataset again, or to
    -- crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Prelude.Maybe RecrawlPolicy,
    -- | A list of custom classifiers that the user has registered. By default,
    -- all built-in classifiers are included in a crawl, but these custom
    -- classifiers always override the default classifiers for a given
    -- classification.
    classifiers :: Prelude.Maybe [Prelude.Text],
    -- | Crawler configuration information. This versioned JSON string allows
    -- users to specify aspects of a crawler\'s behavior. For more information,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | Specifies data lineage configuration settings for the crawler.
    lineageConfiguration :: Prelude.Maybe LineageConfiguration,
    -- | A list of targets to crawl.
    targets :: Prelude.Maybe CrawlerTargets,
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used
    -- by the new crawler to access customer resources.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The table prefix used for catalog tables that are created.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | A description of the new crawler.
    description :: Prelude.Maybe Prelude.Text,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The AWS Glue database where results are stored, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Name of the new crawler.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateCrawler
newUpdateCrawler pName_ =
  UpdateCrawler'
    { schemaChangePolicy =
        Prelude.Nothing,
      recrawlPolicy = Prelude.Nothing,
      classifiers = Prelude.Nothing,
      configuration = Prelude.Nothing,
      lineageConfiguration = Prelude.Nothing,
      targets = Prelude.Nothing,
      role' = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      description = Prelude.Nothing,
      schedule = Prelude.Nothing,
      crawlerSecurityConfiguration = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      name = pName_
    }

-- | The policy for the crawler\'s update and deletion behavior.
updateCrawler_schemaChangePolicy :: Lens.Lens' UpdateCrawler (Prelude.Maybe SchemaChangePolicy)
updateCrawler_schemaChangePolicy = Lens.lens (\UpdateCrawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@UpdateCrawler' {} a -> s {schemaChangePolicy = a} :: UpdateCrawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
updateCrawler_recrawlPolicy :: Lens.Lens' UpdateCrawler (Prelude.Maybe RecrawlPolicy)
updateCrawler_recrawlPolicy = Lens.lens (\UpdateCrawler' {recrawlPolicy} -> recrawlPolicy) (\s@UpdateCrawler' {} a -> s {recrawlPolicy = a} :: UpdateCrawler)

-- | A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
updateCrawler_classifiers :: Lens.Lens' UpdateCrawler (Prelude.Maybe [Prelude.Text])
updateCrawler_classifiers = Lens.lens (\UpdateCrawler' {classifiers} -> classifiers) (\s@UpdateCrawler' {} a -> s {classifiers = a} :: UpdateCrawler) Prelude.. Lens.mapping Prelude._Coerce

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
updateCrawler_configuration :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_configuration = Lens.lens (\UpdateCrawler' {configuration} -> configuration) (\s@UpdateCrawler' {} a -> s {configuration = a} :: UpdateCrawler)

-- | Specifies data lineage configuration settings for the crawler.
updateCrawler_lineageConfiguration :: Lens.Lens' UpdateCrawler (Prelude.Maybe LineageConfiguration)
updateCrawler_lineageConfiguration = Lens.lens (\UpdateCrawler' {lineageConfiguration} -> lineageConfiguration) (\s@UpdateCrawler' {} a -> s {lineageConfiguration = a} :: UpdateCrawler)

-- | A list of targets to crawl.
updateCrawler_targets :: Lens.Lens' UpdateCrawler (Prelude.Maybe CrawlerTargets)
updateCrawler_targets = Lens.lens (\UpdateCrawler' {targets} -> targets) (\s@UpdateCrawler' {} a -> s {targets = a} :: UpdateCrawler)

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role that is used
-- by the new crawler to access customer resources.
updateCrawler_role :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_role = Lens.lens (\UpdateCrawler' {role'} -> role') (\s@UpdateCrawler' {} a -> s {role' = a} :: UpdateCrawler)

-- | The table prefix used for catalog tables that are created.
updateCrawler_tablePrefix :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_tablePrefix = Lens.lens (\UpdateCrawler' {tablePrefix} -> tablePrefix) (\s@UpdateCrawler' {} a -> s {tablePrefix = a} :: UpdateCrawler)

-- | A description of the new crawler.
updateCrawler_description :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_description = Lens.lens (\UpdateCrawler' {description} -> description) (\s@UpdateCrawler' {} a -> s {description = a} :: UpdateCrawler)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
updateCrawler_schedule :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_schedule = Lens.lens (\UpdateCrawler' {schedule} -> schedule) (\s@UpdateCrawler' {} a -> s {schedule = a} :: UpdateCrawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
updateCrawler_crawlerSecurityConfiguration :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_crawlerSecurityConfiguration = Lens.lens (\UpdateCrawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@UpdateCrawler' {} a -> s {crawlerSecurityConfiguration = a} :: UpdateCrawler)

-- | The AWS Glue database where results are stored, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
updateCrawler_databaseName :: Lens.Lens' UpdateCrawler (Prelude.Maybe Prelude.Text)
updateCrawler_databaseName = Lens.lens (\UpdateCrawler' {databaseName} -> databaseName) (\s@UpdateCrawler' {} a -> s {databaseName = a} :: UpdateCrawler)

-- | Name of the new crawler.
updateCrawler_name :: Lens.Lens' UpdateCrawler Prelude.Text
updateCrawler_name = Lens.lens (\UpdateCrawler' {name} -> name) (\s@UpdateCrawler' {} a -> s {name = a} :: UpdateCrawler)

instance Prelude.AWSRequest UpdateCrawler where
  type Rs UpdateCrawler = UpdateCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCrawlerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCrawler

instance Prelude.NFData UpdateCrawler

instance Prelude.ToHeaders UpdateCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateCrawler" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateCrawler where
  toJSON UpdateCrawler' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SchemaChangePolicy" Prelude..=)
              Prelude.<$> schemaChangePolicy,
            ("RecrawlPolicy" Prelude..=)
              Prelude.<$> recrawlPolicy,
            ("Classifiers" Prelude..=) Prelude.<$> classifiers,
            ("Configuration" Prelude..=)
              Prelude.<$> configuration,
            ("LineageConfiguration" Prelude..=)
              Prelude.<$> lineageConfiguration,
            ("Targets" Prelude..=) Prelude.<$> targets,
            ("Role" Prelude..=) Prelude.<$> role',
            ("TablePrefix" Prelude..=) Prelude.<$> tablePrefix,
            ("Description" Prelude..=) Prelude.<$> description,
            ("Schedule" Prelude..=) Prelude.<$> schedule,
            ("CrawlerSecurityConfiguration" Prelude..=)
              Prelude.<$> crawlerSecurityConfiguration,
            ("DatabaseName" Prelude..=) Prelude.<$> databaseName,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath UpdateCrawler where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCrawlerResponse' smart constructor.
data UpdateCrawlerResponse = UpdateCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateCrawlerResponse
newUpdateCrawlerResponse pHttpStatus_ =
  UpdateCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateCrawlerResponse_httpStatus :: Lens.Lens' UpdateCrawlerResponse Prelude.Int
updateCrawlerResponse_httpStatus = Lens.lens (\UpdateCrawlerResponse' {httpStatus} -> httpStatus) (\s@UpdateCrawlerResponse' {} a -> s {httpStatus = a} :: UpdateCrawlerResponse)

instance Prelude.NFData UpdateCrawlerResponse
