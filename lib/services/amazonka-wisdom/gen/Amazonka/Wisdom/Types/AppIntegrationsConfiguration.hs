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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | The Amazon Resource Name (ARN) of the AppIntegrations DataIntegration to
    -- use for ingesting content.
    appIntegrationArn :: Prelude.Text,
    -- | The fields from the source that are made available to your agents in
    -- Wisdom.
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
    -- Make sure to include additional fields. These fields are indexed and
    -- used to source recommendations.
    objectFields :: Prelude.NonEmpty Prelude.Text
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
-- 'appIntegrationArn', 'appIntegrationsConfiguration_appIntegrationArn' - The Amazon Resource Name (ARN) of the AppIntegrations DataIntegration to
-- use for ingesting content.
--
-- 'objectFields', 'appIntegrationsConfiguration_objectFields' - The fields from the source that are made available to your agents in
-- Wisdom.
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
-- Make sure to include additional fields. These fields are indexed and
-- used to source recommendations.
newAppIntegrationsConfiguration ::
  -- | 'appIntegrationArn'
  Prelude.Text ->
  -- | 'objectFields'
  Prelude.NonEmpty Prelude.Text ->
  AppIntegrationsConfiguration
newAppIntegrationsConfiguration
  pAppIntegrationArn_
  pObjectFields_ =
    AppIntegrationsConfiguration'
      { appIntegrationArn =
          pAppIntegrationArn_,
        objectFields =
          Lens.coerced Lens.# pObjectFields_
      }

-- | The Amazon Resource Name (ARN) of the AppIntegrations DataIntegration to
-- use for ingesting content.
appIntegrationsConfiguration_appIntegrationArn :: Lens.Lens' AppIntegrationsConfiguration Prelude.Text
appIntegrationsConfiguration_appIntegrationArn = Lens.lens (\AppIntegrationsConfiguration' {appIntegrationArn} -> appIntegrationArn) (\s@AppIntegrationsConfiguration' {} a -> s {appIntegrationArn = a} :: AppIntegrationsConfiguration)

-- | The fields from the source that are made available to your agents in
-- Wisdom.
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
-- Make sure to include additional fields. These fields are indexed and
-- used to source recommendations.
appIntegrationsConfiguration_objectFields :: Lens.Lens' AppIntegrationsConfiguration (Prelude.NonEmpty Prelude.Text)
appIntegrationsConfiguration_objectFields = Lens.lens (\AppIntegrationsConfiguration' {objectFields} -> objectFields) (\s@AppIntegrationsConfiguration' {} a -> s {objectFields = a} :: AppIntegrationsConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON AppIntegrationsConfiguration where
  parseJSON =
    Data.withObject
      "AppIntegrationsConfiguration"
      ( \x ->
          AppIntegrationsConfiguration'
            Prelude.<$> (x Data..: "appIntegrationArn")
            Prelude.<*> (x Data..: "objectFields")
      )

instance
  Prelude.Hashable
    AppIntegrationsConfiguration
  where
  hashWithSalt _salt AppIntegrationsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` appIntegrationArn
      `Prelude.hashWithSalt` objectFields

instance Prelude.NFData AppIntegrationsConfiguration where
  rnf AppIntegrationsConfiguration' {..} =
    Prelude.rnf appIntegrationArn
      `Prelude.seq` Prelude.rnf objectFields

instance Data.ToJSON AppIntegrationsConfiguration where
  toJSON AppIntegrationsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("appIntegrationArn" Data..= appIntegrationArn),
            Prelude.Just ("objectFields" Data..= objectFields)
          ]
      )
