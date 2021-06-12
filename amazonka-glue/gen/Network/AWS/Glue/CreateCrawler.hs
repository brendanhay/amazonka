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
-- Module      : Network.AWS.Glue.CreateCrawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new crawler with specified targets, role, configuration, and
-- optional schedule. At least one crawl target must be specified, in the
-- @s3Targets@ field, the @jdbcTargets@ field, or the @DynamoDBTargets@
-- field.
module Network.AWS.Glue.CreateCrawler
  ( -- * Creating a Request
    CreateCrawler (..),
    newCreateCrawler,

    -- * Request Lenses
    createCrawler_schemaChangePolicy,
    createCrawler_recrawlPolicy,
    createCrawler_classifiers,
    createCrawler_configuration,
    createCrawler_lineageConfiguration,
    createCrawler_tags,
    createCrawler_tablePrefix,
    createCrawler_description,
    createCrawler_schedule,
    createCrawler_crawlerSecurityConfiguration,
    createCrawler_databaseName,
    createCrawler_name,
    createCrawler_role,
    createCrawler_targets,

    -- * Destructuring the Response
    CreateCrawlerResponse (..),
    newCreateCrawlerResponse,

    -- * Response Lenses
    createCrawlerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
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
    -- | The tags to use with this crawler request. You may use tags to limit
    -- access to the crawler. For more information about tags in AWS Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
    -- in the developer guide.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
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
    -- | The AWS Glue database where results are written, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    databaseName :: Core.Maybe Core.Text,
    -- | Name of the new crawler.
    name :: Core.Text,
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the
    -- new crawler to access customer resources.
    role' :: Core.Text,
    -- | A list of collection of targets to crawl.
    targets :: CrawlerTargets
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCrawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaChangePolicy', 'createCrawler_schemaChangePolicy' - The policy for the crawler\'s update and deletion behavior.
--
-- 'recrawlPolicy', 'createCrawler_recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
--
-- 'classifiers', 'createCrawler_classifiers' - A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
--
-- 'configuration', 'createCrawler_configuration' - Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
--
-- 'lineageConfiguration', 'createCrawler_lineageConfiguration' - Specifies data lineage configuration settings for the crawler.
--
-- 'tags', 'createCrawler_tags' - The tags to use with this crawler request. You may use tags to limit
-- access to the crawler. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
--
-- 'tablePrefix', 'createCrawler_tablePrefix' - The table prefix used for catalog tables that are created.
--
-- 'description', 'createCrawler_description' - A description of the new crawler.
--
-- 'schedule', 'createCrawler_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'crawlerSecurityConfiguration', 'createCrawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'databaseName', 'createCrawler_databaseName' - The AWS Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
--
-- 'name', 'createCrawler_name' - Name of the new crawler.
--
-- 'role'', 'createCrawler_role' - The IAM role or Amazon Resource Name (ARN) of an IAM role used by the
-- new crawler to access customer resources.
--
-- 'targets', 'createCrawler_targets' - A list of collection of targets to crawl.
newCreateCrawler ::
  -- | 'name'
  Core.Text ->
  -- | 'role''
  Core.Text ->
  -- | 'targets'
  CrawlerTargets ->
  CreateCrawler
newCreateCrawler pName_ pRole_ pTargets_ =
  CreateCrawler'
    { schemaChangePolicy = Core.Nothing,
      recrawlPolicy = Core.Nothing,
      classifiers = Core.Nothing,
      configuration = Core.Nothing,
      lineageConfiguration = Core.Nothing,
      tags = Core.Nothing,
      tablePrefix = Core.Nothing,
      description = Core.Nothing,
      schedule = Core.Nothing,
      crawlerSecurityConfiguration = Core.Nothing,
      databaseName = Core.Nothing,
      name = pName_,
      role' = pRole_,
      targets = pTargets_
    }

-- | The policy for the crawler\'s update and deletion behavior.
createCrawler_schemaChangePolicy :: Lens.Lens' CreateCrawler (Core.Maybe SchemaChangePolicy)
createCrawler_schemaChangePolicy = Lens.lens (\CreateCrawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@CreateCrawler' {} a -> s {schemaChangePolicy = a} :: CreateCrawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
createCrawler_recrawlPolicy :: Lens.Lens' CreateCrawler (Core.Maybe RecrawlPolicy)
createCrawler_recrawlPolicy = Lens.lens (\CreateCrawler' {recrawlPolicy} -> recrawlPolicy) (\s@CreateCrawler' {} a -> s {recrawlPolicy = a} :: CreateCrawler)

-- | A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
createCrawler_classifiers :: Lens.Lens' CreateCrawler (Core.Maybe [Core.Text])
createCrawler_classifiers = Lens.lens (\CreateCrawler' {classifiers} -> classifiers) (\s@CreateCrawler' {} a -> s {classifiers = a} :: CreateCrawler) Core.. Lens.mapping Lens._Coerce

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
createCrawler_configuration :: Lens.Lens' CreateCrawler (Core.Maybe Core.Text)
createCrawler_configuration = Lens.lens (\CreateCrawler' {configuration} -> configuration) (\s@CreateCrawler' {} a -> s {configuration = a} :: CreateCrawler)

-- | Specifies data lineage configuration settings for the crawler.
createCrawler_lineageConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe LineageConfiguration)
createCrawler_lineageConfiguration = Lens.lens (\CreateCrawler' {lineageConfiguration} -> lineageConfiguration) (\s@CreateCrawler' {} a -> s {lineageConfiguration = a} :: CreateCrawler)

-- | The tags to use with this crawler request. You may use tags to limit
-- access to the crawler. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
createCrawler_tags :: Lens.Lens' CreateCrawler (Core.Maybe (Core.HashMap Core.Text Core.Text))
createCrawler_tags = Lens.lens (\CreateCrawler' {tags} -> tags) (\s@CreateCrawler' {} a -> s {tags = a} :: CreateCrawler) Core.. Lens.mapping Lens._Coerce

-- | The table prefix used for catalog tables that are created.
createCrawler_tablePrefix :: Lens.Lens' CreateCrawler (Core.Maybe Core.Text)
createCrawler_tablePrefix = Lens.lens (\CreateCrawler' {tablePrefix} -> tablePrefix) (\s@CreateCrawler' {} a -> s {tablePrefix = a} :: CreateCrawler)

-- | A description of the new crawler.
createCrawler_description :: Lens.Lens' CreateCrawler (Core.Maybe Core.Text)
createCrawler_description = Lens.lens (\CreateCrawler' {description} -> description) (\s@CreateCrawler' {} a -> s {description = a} :: CreateCrawler)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
createCrawler_schedule :: Lens.Lens' CreateCrawler (Core.Maybe Core.Text)
createCrawler_schedule = Lens.lens (\CreateCrawler' {schedule} -> schedule) (\s@CreateCrawler' {} a -> s {schedule = a} :: CreateCrawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
createCrawler_crawlerSecurityConfiguration :: Lens.Lens' CreateCrawler (Core.Maybe Core.Text)
createCrawler_crawlerSecurityConfiguration = Lens.lens (\CreateCrawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@CreateCrawler' {} a -> s {crawlerSecurityConfiguration = a} :: CreateCrawler)

-- | The AWS Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
createCrawler_databaseName :: Lens.Lens' CreateCrawler (Core.Maybe Core.Text)
createCrawler_databaseName = Lens.lens (\CreateCrawler' {databaseName} -> databaseName) (\s@CreateCrawler' {} a -> s {databaseName = a} :: CreateCrawler)

-- | Name of the new crawler.
createCrawler_name :: Lens.Lens' CreateCrawler Core.Text
createCrawler_name = Lens.lens (\CreateCrawler' {name} -> name) (\s@CreateCrawler' {} a -> s {name = a} :: CreateCrawler)

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the
-- new crawler to access customer resources.
createCrawler_role :: Lens.Lens' CreateCrawler Core.Text
createCrawler_role = Lens.lens (\CreateCrawler' {role'} -> role') (\s@CreateCrawler' {} a -> s {role' = a} :: CreateCrawler)

-- | A list of collection of targets to crawl.
createCrawler_targets :: Lens.Lens' CreateCrawler CrawlerTargets
createCrawler_targets = Lens.lens (\CreateCrawler' {targets} -> targets) (\s@CreateCrawler' {} a -> s {targets = a} :: CreateCrawler)

instance Core.AWSRequest CreateCrawler where
  type
    AWSResponse CreateCrawler =
      CreateCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCrawlerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCrawler

instance Core.NFData CreateCrawler

instance Core.ToHeaders CreateCrawler where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateCrawler" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCrawler where
  toJSON CreateCrawler' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaChangePolicy" Core..=)
              Core.<$> schemaChangePolicy,
            ("RecrawlPolicy" Core..=) Core.<$> recrawlPolicy,
            ("Classifiers" Core..=) Core.<$> classifiers,
            ("Configuration" Core..=) Core.<$> configuration,
            ("LineageConfiguration" Core..=)
              Core.<$> lineageConfiguration,
            ("Tags" Core..=) Core.<$> tags,
            ("TablePrefix" Core..=) Core.<$> tablePrefix,
            ("Description" Core..=) Core.<$> description,
            ("Schedule" Core..=) Core.<$> schedule,
            ("CrawlerSecurityConfiguration" Core..=)
              Core.<$> crawlerSecurityConfiguration,
            ("DatabaseName" Core..=) Core.<$> databaseName,
            Core.Just ("Name" Core..= name),
            Core.Just ("Role" Core..= role'),
            Core.Just ("Targets" Core..= targets)
          ]
      )

instance Core.ToPath CreateCrawler where
  toPath = Core.const "/"

instance Core.ToQuery CreateCrawler where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCrawlerResponse' smart constructor.
data CreateCrawlerResponse = CreateCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCrawlerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCrawlerResponse_httpStatus' - The response's http status code.
newCreateCrawlerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCrawlerResponse
newCreateCrawlerResponse pHttpStatus_ =
  CreateCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createCrawlerResponse_httpStatus :: Lens.Lens' CreateCrawlerResponse Core.Int
createCrawlerResponse_httpStatus = Lens.lens (\CreateCrawlerResponse' {httpStatus} -> httpStatus) (\s@CreateCrawlerResponse' {} a -> s {httpStatus = a} :: CreateCrawlerResponse)

instance Core.NFData CreateCrawlerResponse
