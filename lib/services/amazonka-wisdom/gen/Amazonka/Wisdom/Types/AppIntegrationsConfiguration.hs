{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Wisdom.Types.AppIntegrationsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.AppIntegrationsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for Amazon AppIntegrations to automatically
-- ingest content.
--
-- /See:/ 'newAppIntegrationsConfiguration' smart constructor.
data AppIntegrationsConfiguration = AppIntegrationsConfiguration'
  { -- | The fields from the source that are made available to your agents in
    -- Wisdom. Optional if ObjectConfiguration is included in the provided
    -- DataIntegration.
    --
    -- -   For
    --     <https://developer.salesforce.com/docs/atlas.en-us.knowledge_dev.meta/knowledge_dev/sforce_api_objects_knowledge__kav.htm Salesforce>,
    --     you must include at least @Id@, @ArticleNumber@, @VersionNumber@,
    --     @Title@, @PublishStatus@, and @IsDeleted@.
    --
    -- -   For
    --     <https://developer.servicenow.com/dev.do#!/reference/api/rome/rest/knowledge-management-api ServiceNow>,
    --     you must include at least @number@, @short_description@,
    --     @sys_mod_count@, @workflow_state@, and @active@.
    --
    -- -   For
    --     <https://developer.zendesk.com/api-reference/help_center/help-center-api/articles/ Zendesk>,
    --     you must include at least @id@, @title@, @updated_at@, and @draft@.
    --
    -- Make sure to include additional fields. These fields are indexed and
    -- used to source recommendations.
    objectFields :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the AppIntegrations DataIntegration to
    -- use for ingesting content.
    --
    -- -   For
    --     <https://developer.salesforce.com/docs/atlas.en-us.knowledge_dev.meta/knowledge_dev/sforce_api_objects_knowledge__kav.htm Salesforce>,
    --     your AppIntegrations DataIntegration must have an
    --     ObjectConfiguration if objectFields is not provided, including at
    --     least @Id@, @ArticleNumber@, @VersionNumber@, @Title@,
    --     @PublishStatus@, and @IsDeleted@ as source fields.
    --
    -- -   For
    --     <https://developer.servicenow.com/dev.do#!/reference/api/rome/rest/knowledge-management-api ServiceNow>,
    --     your AppIntegrations DataIntegration must have an
    --     ObjectConfiguration if objectFields is not provided, including at
    --     least @number@, @short_description@, @sys_mod_count@,
    --     @workflow_state@, and @active@ as source fields.
    --
    -- -   For
    --     <https://developer.zendesk.com/api-reference/help_center/help-center-api/articles/ Zendesk>,
    --     your AppIntegrations DataIntegration must have an
    --     ObjectConfiguration if @objectFields@ is not provided, including at
    --     least @id@, @title@, @updated_at@, and @draft@ as source fields.
    --
    -- -   For
    --     <https://learn.microsoft.com/en-us/sharepoint/dev/sp-add-ins/sharepoint-net-server-csom-jsom-and-rest-api-index SharePoint>,
    --     your AppIntegrations DataIntegration must have a FileConfiguration,
    --     including only file extensions that are among @docx@, @pdf@, @html@,
    --     @htm@, and @txt@.
    appIntegrationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppIntegrationsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectFields', 'appIntegrationsConfiguration_objectFields' - The fields from the source that are made available to your agents in
-- Wisdom. Optional if ObjectConfiguration is included in the provided
-- DataIntegration.
--
-- -   For
--     <https://developer.salesforce.com/docs/atlas.en-us.knowledge_dev.meta/knowledge_dev/sforce_api_objects_knowledge__kav.htm Salesforce>,
--     you must include at least @Id@, @ArticleNumber@, @VersionNumber@,
--     @Title@, @PublishStatus@, and @IsDeleted@.
--
-- -   For
--     <https://developer.servicenow.com/dev.do#!/reference/api/rome/rest/knowledge-management-api ServiceNow>,
--     you must include at least @number@, @short_description@,
--     @sys_mod_count@, @workflow_state@, and @active@.
--
-- -   For
--     <https://developer.zendesk.com/api-reference/help_center/help-center-api/articles/ Zendesk>,
--     you must include at least @id@, @title@, @updated_at@, and @draft@.
--
-- Make sure to include additional fields. These fields are indexed and
-- used to source recommendations.
--
-- 'appIntegrationArn', 'appIntegrationsConfiguration_appIntegrationArn' - The Amazon Resource Name (ARN) of the AppIntegrations DataIntegration to
-- use for ingesting content.
--
-- -   For
--     <https://developer.salesforce.com/docs/atlas.en-us.knowledge_dev.meta/knowledge_dev/sforce_api_objects_knowledge__kav.htm Salesforce>,
--     your AppIntegrations DataIntegration must have an
--     ObjectConfiguration if objectFields is not provided, including at
--     least @Id@, @ArticleNumber@, @VersionNumber@, @Title@,
--     @PublishStatus@, and @IsDeleted@ as source fields.
--
-- -   For
--     <https://developer.servicenow.com/dev.do#!/reference/api/rome/rest/knowledge-management-api ServiceNow>,
--     your AppIntegrations DataIntegration must have an
--     ObjectConfiguration if objectFields is not provided, including at
--     least @number@, @short_description@, @sys_mod_count@,
--     @workflow_state@, and @active@ as source fields.
--
-- -   For
--     <https://developer.zendesk.com/api-reference/help_center/help-center-api/articles/ Zendesk>,
--     your AppIntegrations DataIntegration must have an
--     ObjectConfiguration if @objectFields@ is not provided, including at
--     least @id@, @title@, @updated_at@, and @draft@ as source fields.
--
-- -   For
--     <https://learn.microsoft.com/en-us/sharepoint/dev/sp-add-ins/sharepoint-net-server-csom-jsom-and-rest-api-index SharePoint>,
--     your AppIntegrations DataIntegration must have a FileConfiguration,
--     including only file extensions that are among @docx@, @pdf@, @html@,
--     @htm@, and @txt@.
newAppIntegrationsConfiguration ::
  -- | 'appIntegrationArn'
  Prelude.Text ->
  AppIntegrationsConfiguration
newAppIntegrationsConfiguration pAppIntegrationArn_ =
  AppIntegrationsConfiguration'
    { objectFields =
        Prelude.Nothing,
      appIntegrationArn = pAppIntegrationArn_
    }

-- | The fields from the source that are made available to your agents in
-- Wisdom. Optional if ObjectConfiguration is included in the provided
-- DataIntegration.
--
-- -   For
--     <https://developer.salesforce.com/docs/atlas.en-us.knowledge_dev.meta/knowledge_dev/sforce_api_objects_knowledge__kav.htm Salesforce>,
--     you must include at least @Id@, @ArticleNumber@, @VersionNumber@,
--     @Title@, @PublishStatus@, and @IsDeleted@.
--
-- -   For
--     <https://developer.servicenow.com/dev.do#!/reference/api/rome/rest/knowledge-management-api ServiceNow>,
--     you must include at least @number@, @short_description@,
--     @sys_mod_count@, @workflow_state@, and @active@.
--
-- -   For
--     <https://developer.zendesk.com/api-reference/help_center/help-center-api/articles/ Zendesk>,
--     you must include at least @id@, @title@, @updated_at@, and @draft@.
--
-- Make sure to include additional fields. These fields are indexed and
-- used to source recommendations.
appIntegrationsConfiguration_objectFields :: Lens.Lens' AppIntegrationsConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
appIntegrationsConfiguration_objectFields = Lens.lens (\AppIntegrationsConfiguration' {objectFields} -> objectFields) (\s@AppIntegrationsConfiguration' {} a -> s {objectFields = a} :: AppIntegrationsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the AppIntegrations DataIntegration to
-- use for ingesting content.
--
-- -   For
--     <https://developer.salesforce.com/docs/atlas.en-us.knowledge_dev.meta/knowledge_dev/sforce_api_objects_knowledge__kav.htm Salesforce>,
--     your AppIntegrations DataIntegration must have an
--     ObjectConfiguration if objectFields is not provided, including at
--     least @Id@, @ArticleNumber@, @VersionNumber@, @Title@,
--     @PublishStatus@, and @IsDeleted@ as source fields.
--
-- -   For
--     <https://developer.servicenow.com/dev.do#!/reference/api/rome/rest/knowledge-management-api ServiceNow>,
--     your AppIntegrations DataIntegration must have an
--     ObjectConfiguration if objectFields is not provided, including at
--     least @number@, @short_description@, @sys_mod_count@,
--     @workflow_state@, and @active@ as source fields.
--
-- -   For
--     <https://developer.zendesk.com/api-reference/help_center/help-center-api/articles/ Zendesk>,
--     your AppIntegrations DataIntegration must have an
--     ObjectConfiguration if @objectFields@ is not provided, including at
--     least @id@, @title@, @updated_at@, and @draft@ as source fields.
--
-- -   For
--     <https://learn.microsoft.com/en-us/sharepoint/dev/sp-add-ins/sharepoint-net-server-csom-jsom-and-rest-api-index SharePoint>,
--     your AppIntegrations DataIntegration must have a FileConfiguration,
--     including only file extensions that are among @docx@, @pdf@, @html@,
--     @htm@, and @txt@.
appIntegrationsConfiguration_appIntegrationArn :: Lens.Lens' AppIntegrationsConfiguration Prelude.Text
appIntegrationsConfiguration_appIntegrationArn = Lens.lens (\AppIntegrationsConfiguration' {appIntegrationArn} -> appIntegrationArn) (\s@AppIntegrationsConfiguration' {} a -> s {appIntegrationArn = a} :: AppIntegrationsConfiguration)

instance Data.FromJSON AppIntegrationsConfiguration where
  parseJSON =
    Data.withObject
      "AppIntegrationsConfiguration"
      ( \x ->
          AppIntegrationsConfiguration'
            Prelude.<$> (x Data..:? "objectFields")
            Prelude.<*> (x Data..: "appIntegrationArn")
      )

instance
  Prelude.Hashable
    AppIntegrationsConfiguration
  where
  hashWithSalt _salt AppIntegrationsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` objectFields
      `Prelude.hashWithSalt` appIntegrationArn

instance Prelude.NFData AppIntegrationsConfiguration where
  rnf AppIntegrationsConfiguration' {..} =
    Prelude.rnf objectFields
      `Prelude.seq` Prelude.rnf appIntegrationArn

instance Data.ToJSON AppIntegrationsConfiguration where
  toJSON AppIntegrationsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("objectFields" Data..=) Prelude.<$> objectFields,
            Prelude.Just
              ("appIntegrationArn" Data..= appIntegrationArn)
          ]
      )
