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
-- Module      : Amazonka.Glue.CreateCrawler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new crawler with specified targets, role, configuration, and
-- optional schedule. At least one crawl target must be specified, in the
-- @s3Targets@ field, the @jdbcTargets@ field, or the @DynamoDBTargets@
-- field.
module Amazonka.Glue.CreateCrawler
  ( -- * Creating a Request
    CreateCrawler (..),
    newCreateCrawler,

    -- * Request Lenses
    createCrawler_classifiers,
    createCrawler_configuration,
    createCrawler_crawlerSecurityConfiguration,
    createCrawler_databaseName,
    createCrawler_description,
    createCrawler_lakeFormationConfiguration,
    createCrawler_lineageConfiguration,
    createCrawler_recrawlPolicy,
    createCrawler_schedule,
    createCrawler_schemaChangePolicy,
    createCrawler_tablePrefix,
    createCrawler_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCrawler' smart constructor.
data CreateCrawler = CreateCrawler'
  { -- | A list of custom classifiers that the user has registered. By default,
    -- all built-in classifiers are included in a crawl, but these custom
    -- classifiers always override the default classifiers for a given
    -- classification.
    classifiers :: Prelude.Maybe [Prelude.Text],
    -- | Crawler configuration information. This versioned JSON string allows
    -- users to specify aspects of a crawler\'s behavior. For more information,
    -- see
    -- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The name of the @SecurityConfiguration@ structure to be used by this
    -- crawler.
    crawlerSecurityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The Glue database where results are written, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A description of the new crawler.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies Lake Formation configuration settings for the crawler.
    lakeFormationConfiguration :: Prelude.Maybe LakeFormationConfiguration,
    -- | Specifies data lineage configuration settings for the crawler.
    lineageConfiguration :: Prelude.Maybe LineageConfiguration,
    -- | A policy that specifies whether to crawl the entire dataset again, or to
    -- crawl only folders that were added since the last crawler run.
    recrawlPolicy :: Prelude.Maybe RecrawlPolicy,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The policy for the crawler\'s update and deletion behavior.
    schemaChangePolicy :: Prelude.Maybe SchemaChangePolicy,
    -- | The table prefix used for catalog tables that are created.
    tablePrefix :: Prelude.Maybe Prelude.Text,
    -- | The tags to use with this crawler request. You may use tags to limit
    -- access to the crawler. For more information about tags in Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'classifiers', 'createCrawler_classifiers' - A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
--
-- 'configuration', 'createCrawler_configuration' - Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
--
-- 'crawlerSecurityConfiguration', 'createCrawler_crawlerSecurityConfiguration' - The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
--
-- 'databaseName', 'createCrawler_databaseName' - The Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
--
-- 'description', 'createCrawler_description' - A description of the new crawler.
--
-- 'lakeFormationConfiguration', 'createCrawler_lakeFormationConfiguration' - Specifies Lake Formation configuration settings for the crawler.
--
-- 'lineageConfiguration', 'createCrawler_lineageConfiguration' - Specifies data lineage configuration settings for the crawler.
--
-- 'recrawlPolicy', 'createCrawler_recrawlPolicy' - A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
--
-- 'schedule', 'createCrawler_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'schemaChangePolicy', 'createCrawler_schemaChangePolicy' - The policy for the crawler\'s update and deletion behavior.
--
-- 'tablePrefix', 'createCrawler_tablePrefix' - The table prefix used for catalog tables that are created.
--
-- 'tags', 'createCrawler_tags' - The tags to use with this crawler request. You may use tags to limit
-- access to the crawler. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
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
    { classifiers = Prelude.Nothing,
      configuration = Prelude.Nothing,
      crawlerSecurityConfiguration = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      description = Prelude.Nothing,
      lakeFormationConfiguration = Prelude.Nothing,
      lineageConfiguration = Prelude.Nothing,
      recrawlPolicy = Prelude.Nothing,
      schedule = Prelude.Nothing,
      schemaChangePolicy = Prelude.Nothing,
      tablePrefix = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      role' = pRole_,
      targets = pTargets_
    }

-- | A list of custom classifiers that the user has registered. By default,
-- all built-in classifiers are included in a crawl, but these custom
-- classifiers always override the default classifiers for a given
-- classification.
createCrawler_classifiers :: Lens.Lens' CreateCrawler (Prelude.Maybe [Prelude.Text])
createCrawler_classifiers = Lens.lens (\CreateCrawler' {classifiers} -> classifiers) (\s@CreateCrawler' {} a -> s {classifiers = a} :: CreateCrawler) Prelude.. Lens.mapping Lens.coerced

-- | Crawler configuration information. This versioned JSON string allows
-- users to specify aspects of a crawler\'s behavior. For more information,
-- see
-- <https://docs.aws.amazon.com/glue/latest/dg/crawler-configuration.html Setting crawler configuration options>.
createCrawler_configuration :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_configuration = Lens.lens (\CreateCrawler' {configuration} -> configuration) (\s@CreateCrawler' {} a -> s {configuration = a} :: CreateCrawler)

-- | The name of the @SecurityConfiguration@ structure to be used by this
-- crawler.
createCrawler_crawlerSecurityConfiguration :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_crawlerSecurityConfiguration = Lens.lens (\CreateCrawler' {crawlerSecurityConfiguration} -> crawlerSecurityConfiguration) (\s@CreateCrawler' {} a -> s {crawlerSecurityConfiguration = a} :: CreateCrawler)

-- | The Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
createCrawler_databaseName :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_databaseName = Lens.lens (\CreateCrawler' {databaseName} -> databaseName) (\s@CreateCrawler' {} a -> s {databaseName = a} :: CreateCrawler)

-- | A description of the new crawler.
createCrawler_description :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_description = Lens.lens (\CreateCrawler' {description} -> description) (\s@CreateCrawler' {} a -> s {description = a} :: CreateCrawler)

-- | Specifies Lake Formation configuration settings for the crawler.
createCrawler_lakeFormationConfiguration :: Lens.Lens' CreateCrawler (Prelude.Maybe LakeFormationConfiguration)
createCrawler_lakeFormationConfiguration = Lens.lens (\CreateCrawler' {lakeFormationConfiguration} -> lakeFormationConfiguration) (\s@CreateCrawler' {} a -> s {lakeFormationConfiguration = a} :: CreateCrawler)

-- | Specifies data lineage configuration settings for the crawler.
createCrawler_lineageConfiguration :: Lens.Lens' CreateCrawler (Prelude.Maybe LineageConfiguration)
createCrawler_lineageConfiguration = Lens.lens (\CreateCrawler' {lineageConfiguration} -> lineageConfiguration) (\s@CreateCrawler' {} a -> s {lineageConfiguration = a} :: CreateCrawler)

-- | A policy that specifies whether to crawl the entire dataset again, or to
-- crawl only folders that were added since the last crawler run.
createCrawler_recrawlPolicy :: Lens.Lens' CreateCrawler (Prelude.Maybe RecrawlPolicy)
createCrawler_recrawlPolicy = Lens.lens (\CreateCrawler' {recrawlPolicy} -> recrawlPolicy) (\s@CreateCrawler' {} a -> s {recrawlPolicy = a} :: CreateCrawler)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
createCrawler_schedule :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_schedule = Lens.lens (\CreateCrawler' {schedule} -> schedule) (\s@CreateCrawler' {} a -> s {schedule = a} :: CreateCrawler)

-- | The policy for the crawler\'s update and deletion behavior.
createCrawler_schemaChangePolicy :: Lens.Lens' CreateCrawler (Prelude.Maybe SchemaChangePolicy)
createCrawler_schemaChangePolicy = Lens.lens (\CreateCrawler' {schemaChangePolicy} -> schemaChangePolicy) (\s@CreateCrawler' {} a -> s {schemaChangePolicy = a} :: CreateCrawler)

-- | The table prefix used for catalog tables that are created.
createCrawler_tablePrefix :: Lens.Lens' CreateCrawler (Prelude.Maybe Prelude.Text)
createCrawler_tablePrefix = Lens.lens (\CreateCrawler' {tablePrefix} -> tablePrefix) (\s@CreateCrawler' {} a -> s {tablePrefix = a} :: CreateCrawler)

-- | The tags to use with this crawler request. You may use tags to limit
-- access to the crawler. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
createCrawler_tags :: Lens.Lens' CreateCrawler (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCrawler_tags = Lens.lens (\CreateCrawler' {tags} -> tags) (\s@CreateCrawler' {} a -> s {tags = a} :: CreateCrawler) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCrawlerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCrawler where
  hashWithSalt _salt CreateCrawler' {..} =
    _salt
      `Prelude.hashWithSalt` classifiers
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` crawlerSecurityConfiguration
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lakeFormationConfiguration
      `Prelude.hashWithSalt` lineageConfiguration
      `Prelude.hashWithSalt` recrawlPolicy
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` schemaChangePolicy
      `Prelude.hashWithSalt` tablePrefix
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` targets

instance Prelude.NFData CreateCrawler where
  rnf CreateCrawler' {..} =
    Prelude.rnf classifiers
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf crawlerSecurityConfiguration
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lakeFormationConfiguration
      `Prelude.seq` Prelude.rnf lineageConfiguration
      `Prelude.seq` Prelude.rnf recrawlPolicy
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf schemaChangePolicy
      `Prelude.seq` Prelude.rnf tablePrefix
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf targets

instance Data.ToHeaders CreateCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateCrawler" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCrawler where
  toJSON CreateCrawler' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Classifiers" Data..=) Prelude.<$> classifiers,
            ("Configuration" Data..=) Prelude.<$> configuration,
            ("CrawlerSecurityConfiguration" Data..=)
              Prelude.<$> crawlerSecurityConfiguration,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("Description" Data..=) Prelude.<$> description,
            ("LakeFormationConfiguration" Data..=)
              Prelude.<$> lakeFormationConfiguration,
            ("LineageConfiguration" Data..=)
              Prelude.<$> lineageConfiguration,
            ("RecrawlPolicy" Data..=) Prelude.<$> recrawlPolicy,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("SchemaChangePolicy" Data..=)
              Prelude.<$> schemaChangePolicy,
            ("TablePrefix" Data..=) Prelude.<$> tablePrefix,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("Targets" Data..= targets)
          ]
      )

instance Data.ToPath CreateCrawler where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCrawler where
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

instance Prelude.NFData CreateCrawlerResponse where
  rnf CreateCrawlerResponse' {..} =
    Prelude.rnf httpStatus
