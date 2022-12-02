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
-- Module      : Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.SourceConnectorType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Structure holding all @APPFLOW_INTEGRATION@ specific workflow
-- attributes.
--
-- /See:/ 'newAppflowIntegrationWorkflowAttributes' smart constructor.
data AppflowIntegrationWorkflowAttributes = AppflowIntegrationWorkflowAttributes'
  { -- | The Amazon Resource Name (ARN) of the IAM role. Customer Profiles
    -- assumes this role to create resources on your behalf as part of workflow
    -- execution.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the source connector type, such as Salesforce, ServiceNow, and
    -- Marketo. Indicates source of ingestion.
    sourceConnectorType :: SourceConnectorType,
    -- | The name of the AppFlow connector profile used for ingestion.
    connectorProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppflowIntegrationWorkflowAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'appflowIntegrationWorkflowAttributes_roleArn' - The Amazon Resource Name (ARN) of the IAM role. Customer Profiles
-- assumes this role to create resources on your behalf as part of workflow
-- execution.
--
-- 'sourceConnectorType', 'appflowIntegrationWorkflowAttributes_sourceConnectorType' - Specifies the source connector type, such as Salesforce, ServiceNow, and
-- Marketo. Indicates source of ingestion.
--
-- 'connectorProfileName', 'appflowIntegrationWorkflowAttributes_connectorProfileName' - The name of the AppFlow connector profile used for ingestion.
newAppflowIntegrationWorkflowAttributes ::
  -- | 'sourceConnectorType'
  SourceConnectorType ->
  -- | 'connectorProfileName'
  Prelude.Text ->
  AppflowIntegrationWorkflowAttributes
newAppflowIntegrationWorkflowAttributes
  pSourceConnectorType_
  pConnectorProfileName_ =
    AppflowIntegrationWorkflowAttributes'
      { roleArn =
          Prelude.Nothing,
        sourceConnectorType =
          pSourceConnectorType_,
        connectorProfileName =
          pConnectorProfileName_
      }

-- | The Amazon Resource Name (ARN) of the IAM role. Customer Profiles
-- assumes this role to create resources on your behalf as part of workflow
-- execution.
appflowIntegrationWorkflowAttributes_roleArn :: Lens.Lens' AppflowIntegrationWorkflowAttributes (Prelude.Maybe Prelude.Text)
appflowIntegrationWorkflowAttributes_roleArn = Lens.lens (\AppflowIntegrationWorkflowAttributes' {roleArn} -> roleArn) (\s@AppflowIntegrationWorkflowAttributes' {} a -> s {roleArn = a} :: AppflowIntegrationWorkflowAttributes)

-- | Specifies the source connector type, such as Salesforce, ServiceNow, and
-- Marketo. Indicates source of ingestion.
appflowIntegrationWorkflowAttributes_sourceConnectorType :: Lens.Lens' AppflowIntegrationWorkflowAttributes SourceConnectorType
appflowIntegrationWorkflowAttributes_sourceConnectorType = Lens.lens (\AppflowIntegrationWorkflowAttributes' {sourceConnectorType} -> sourceConnectorType) (\s@AppflowIntegrationWorkflowAttributes' {} a -> s {sourceConnectorType = a} :: AppflowIntegrationWorkflowAttributes)

-- | The name of the AppFlow connector profile used for ingestion.
appflowIntegrationWorkflowAttributes_connectorProfileName :: Lens.Lens' AppflowIntegrationWorkflowAttributes Prelude.Text
appflowIntegrationWorkflowAttributes_connectorProfileName = Lens.lens (\AppflowIntegrationWorkflowAttributes' {connectorProfileName} -> connectorProfileName) (\s@AppflowIntegrationWorkflowAttributes' {} a -> s {connectorProfileName = a} :: AppflowIntegrationWorkflowAttributes)

instance
  Data.FromJSON
    AppflowIntegrationWorkflowAttributes
  where
  parseJSON =
    Data.withObject
      "AppflowIntegrationWorkflowAttributes"
      ( \x ->
          AppflowIntegrationWorkflowAttributes'
            Prelude.<$> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..: "SourceConnectorType")
            Prelude.<*> (x Data..: "ConnectorProfileName")
      )

instance
  Prelude.Hashable
    AppflowIntegrationWorkflowAttributes
  where
  hashWithSalt
    _salt
    AppflowIntegrationWorkflowAttributes' {..} =
      _salt `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` sourceConnectorType
        `Prelude.hashWithSalt` connectorProfileName

instance
  Prelude.NFData
    AppflowIntegrationWorkflowAttributes
  where
  rnf AppflowIntegrationWorkflowAttributes' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sourceConnectorType
      `Prelude.seq` Prelude.rnf connectorProfileName
