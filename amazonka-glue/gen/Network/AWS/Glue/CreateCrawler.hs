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
    createCrawler_description,
    createCrawler_tablePrefix,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
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
    -- | The tags to use with this crawler request. You may use tags to limit
    -- access to the crawler. For more information about tags in Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the new crawler.
    description :: Prelude.Maybe Prelude.Text,
    -- | The table prefix used for catalog tables that are created.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The Glue database where results are written, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | Name of the new crawler.
    name :: Prelude.Text,
    -- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the
    -- new crawler to access customer resources.
    role' :: Prelude.Text,
    -- | A list of collection of targets to crawl.
    targets :: CrawlerTargets
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- access to the crawler. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
--
-- 'description', 'createCrawler_description' - A description of the new crawler.
--
-- 'tablePrefix', 'createCrawler_tablePrefix' - The table prefix used for catalog tables that are created.
--
-- 'schedule', 'createCrawler_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'crawlerSecurityConfiguration', 'createCrawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'databaseName', 'createCrawler_databaseName' - The Glue database where results are written, such as:
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
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'targets'
  CrawlerTargets ->
  CreateCrawler
newCreateCrawler pName_ pRole_ pTargets_ =
  CreateCrawler'
    { schemaChangePolicy =
        Prelude.Nothing,
      recrawlPolicy = Prelude.Nothing,
      classifiers = Prelude.Nothing,
      configuration = Prelude.Nothing,
      lineageConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      schedule = Prelude.Nothing,
      crawlerSecurityConfiguration = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      name = pName_,
      role' = pRole_,
      targets = pTargets_
    }

-- | The policy for the crawler\'s update and deletion behavior.
createCrawler_schemaChangePolicy :: Lens.Lens' CreateCrawler (Prelude.Maybe SchemaChangePolicy)
createCrawler_schemaChangePolicy = Lens.lens (\CreateCrawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@CreateCrawler' {} a -> s {schemaChangePolicy = a} :: CreateCrawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
createCrawler_recrawlPolicy :: Lens.Lens' CreateCrawler (Prelude.Maybe RecrawlPolicy)
createCrawler_recrawlPolicy = Lens.lens (\CreateCrawler' {recrawlPolicy} -> recrawlPolicy) (\s@CreateCrawler' {} a -> s {recrawlPolicy = a} :: CreateCrawler)

-- | A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
createCrawler_classifiers :: Lens.Lens' CreateCrawler (Prelude.Maybe [Prelude.Text])
createCrawler_classifiers = Lens.lens (\CreateCrawler' {classifiers} -> classifiers) (\s@CreateCrawler' {} a -> s {classifiers = a} :: CreateCrawler) Prelude.. Lens.mapping Lens._Coerce

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Configuring a Crawler>.
createCrawler_configuration :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_configuration = Lens.lens (\CreateCrawler' {configuration} -> configuration) (\s@CreateCrawler' {} a -> s {configuration = a} :: CreateCrawler)

-- | Specifies data lineage configuration settings for the crawler.
createCrawler_lineageConfiguration :: Lens.Lens' CreateCrawler (Prelude.Maybe LineageConfiguration)
createCrawler_lineageConfiguration = Lens.lens (\CreateCrawler' {lineageConfiguration} -> lineageConfiguration) (\s@CreateCrawler' {} a -> s {lineageConfiguration = a} :: CreateCrawler)

-- | The tags to use with this crawler request. You may use tags to limit
-- access to the crawler. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
createCrawler_tags :: Lens.Lens' CreateCrawler (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCrawler_tags = Lens.lens (\CreateCrawler' {tags} -> tags) (\s@CreateCrawler' {} a -> s {tags = a} :: CreateCrawler) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the new crawler.
createCrawler_description :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_description = Lens.lens (\CreateCrawler' {description} -> description) (\s@CreateCrawler' {} a -> s {description = a} :: CreateCrawler)

-- | The table prefix used for catalog tables that are created.
createCrawler_tablePrefix :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_tablePrefix = Lens.lens (\CreateCrawler' {tablePrefix} -> tablePrefix) (\s@CreateCrawler' {} a -> s {tablePrefix = a} :: CreateCrawler)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
createCrawler_schedule :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_schedule = Lens.lens (\CreateCrawler' {schedule} -> schedule) (\s@CreateCrawler' {} a -> s {schedule = a} :: CreateCrawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
createCrawler_crawlerSecurityConfiguration :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_crawlerSecurityConfiguration = Lens.lens (\CreateCrawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@CreateCrawler' {} a -> s {crawlerSecurityConfiguration = a} :: CreateCrawler)

-- | The Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
createCrawler_databaseName :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_databaseName = Lens.lens (\CreateCrawler' {databaseName} -> databaseName) (\s@CreateCrawler' {} a -> s {databaseName = a} :: CreateCrawler)

-- | Name of the new crawler.
createCrawler_name :: Lens.Lens' CreateCrawler Prelude.Text
createCrawler_name = Lens.lens (\CreateCrawler' {name} -> name) (\s@CreateCrawler' {} a -> s {name = a} :: CreateCrawler)

-- | The IAM role or Amazon Resource Name (ARN) of an IAM role used by the
-- new crawler to access customer resources.
createCrawler_role :: Lens.Lens' CreateCrawler Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCrawler

instance Prelude.NFData CreateCrawler

instance Core.ToHeaders CreateCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateCrawler" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCrawler where
  toJSON CreateCrawler' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaChangePolicy" Core..=)
              Prelude.<$> schemaChangePolicy,
            ("RecrawlPolicy" Core..=) Prelude.<$> recrawlPolicy,
            ("Classifiers" Core..=) Prelude.<$> classifiers,
            ("Configuration" Core..=) Prelude.<$> configuration,
            ("LineageConfiguration" Core..=)
              Prelude.<$> lineageConfiguration,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("TablePrefix" Core..=) Prelude.<$> tablePrefix,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("CrawlerSecurityConfiguration" Core..=)
              Prelude.<$> crawlerSecurityConfiguration,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Role" Core..= role'),
            Prelude.Just ("Targets" Core..= targets)
          ]
      )

instance Core.ToPath CreateCrawler where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCrawlerResponse' smart constructor.
data CreateCrawlerResponse = CreateCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateCrawlerResponse
newCreateCrawlerResponse pHttpStatus_ =
  CreateCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createCrawlerResponse_httpStatus :: Lens.Lens' CreateCrawlerResponse Prelude.Int
createCrawlerResponse_httpStatus = Lens.lens (\CreateCrawlerResponse' {httpStatus} -> httpStatus) (\s@CreateCrawlerResponse' {} a -> s {httpStatus = a} :: CreateCrawlerResponse)

instance Prelude.NFData CreateCrawlerResponse
