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
-- Module      : Amazonka.CustomerProfiles.Types.SourceFlowConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.SourceFlowConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.IncrementalPullConfig
import Amazonka.CustomerProfiles.Types.SourceConnectorProperties
import Amazonka.CustomerProfiles.Types.SourceConnectorType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the configuration of the source connector
-- used in the flow.
--
-- /See:/ 'newSourceFlowConfig' smart constructor.
data SourceFlowConfig = SourceFlowConfig'
  { -- | The name of the AppFlow connector profile. This name must be unique for
    -- each connector profile in the AWS account.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | Defines the configuration for a scheduled incremental data pull. If a
    -- valid configuration is provided, the fields specified in the
    -- configuration are used when querying for the incremental data pull.
    incrementalPullConfig :: Prelude.Maybe IncrementalPullConfig,
    -- | The type of connector, such as Salesforce, Marketo, and so on.
    connectorType :: SourceConnectorType,
    -- | Specifies the information that is required to query a particular source
    -- connector.
    sourceConnectorProperties :: SourceConnectorProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceFlowConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorProfileName', 'sourceFlowConfig_connectorProfileName' - The name of the AppFlow connector profile. This name must be unique for
-- each connector profile in the AWS account.
--
-- 'incrementalPullConfig', 'sourceFlowConfig_incrementalPullConfig' - Defines the configuration for a scheduled incremental data pull. If a
-- valid configuration is provided, the fields specified in the
-- configuration are used when querying for the incremental data pull.
--
-- 'connectorType', 'sourceFlowConfig_connectorType' - The type of connector, such as Salesforce, Marketo, and so on.
--
-- 'sourceConnectorProperties', 'sourceFlowConfig_sourceConnectorProperties' - Specifies the information that is required to query a particular source
-- connector.
newSourceFlowConfig ::
  -- | 'connectorType'
  SourceConnectorType ->
  -- | 'sourceConnectorProperties'
  SourceConnectorProperties ->
  SourceFlowConfig
newSourceFlowConfig
  pConnectorType_
  pSourceConnectorProperties_ =
    SourceFlowConfig'
      { connectorProfileName =
          Prelude.Nothing,
        incrementalPullConfig = Prelude.Nothing,
        connectorType = pConnectorType_,
        sourceConnectorProperties =
          pSourceConnectorProperties_
      }

-- | The name of the AppFlow connector profile. This name must be unique for
-- each connector profile in the AWS account.
sourceFlowConfig_connectorProfileName :: Lens.Lens' SourceFlowConfig (Prelude.Maybe Prelude.Text)
sourceFlowConfig_connectorProfileName = Lens.lens (\SourceFlowConfig' {connectorProfileName} -> connectorProfileName) (\s@SourceFlowConfig' {} a -> s {connectorProfileName = a} :: SourceFlowConfig)

-- | Defines the configuration for a scheduled incremental data pull. If a
-- valid configuration is provided, the fields specified in the
-- configuration are used when querying for the incremental data pull.
sourceFlowConfig_incrementalPullConfig :: Lens.Lens' SourceFlowConfig (Prelude.Maybe IncrementalPullConfig)
sourceFlowConfig_incrementalPullConfig = Lens.lens (\SourceFlowConfig' {incrementalPullConfig} -> incrementalPullConfig) (\s@SourceFlowConfig' {} a -> s {incrementalPullConfig = a} :: SourceFlowConfig)

-- | The type of connector, such as Salesforce, Marketo, and so on.
sourceFlowConfig_connectorType :: Lens.Lens' SourceFlowConfig SourceConnectorType
sourceFlowConfig_connectorType = Lens.lens (\SourceFlowConfig' {connectorType} -> connectorType) (\s@SourceFlowConfig' {} a -> s {connectorType = a} :: SourceFlowConfig)

-- | Specifies the information that is required to query a particular source
-- connector.
sourceFlowConfig_sourceConnectorProperties :: Lens.Lens' SourceFlowConfig SourceConnectorProperties
sourceFlowConfig_sourceConnectorProperties = Lens.lens (\SourceFlowConfig' {sourceConnectorProperties} -> sourceConnectorProperties) (\s@SourceFlowConfig' {} a -> s {sourceConnectorProperties = a} :: SourceFlowConfig)

instance Prelude.Hashable SourceFlowConfig where
  hashWithSalt _salt SourceFlowConfig' {..} =
    _salt
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` incrementalPullConfig
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` sourceConnectorProperties

instance Prelude.NFData SourceFlowConfig where
  rnf SourceFlowConfig' {..} =
    Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf incrementalPullConfig
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf sourceConnectorProperties

instance Data.ToJSON SourceFlowConfig where
  toJSON SourceFlowConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectorProfileName" Data..=)
              Prelude.<$> connectorProfileName,
            ("IncrementalPullConfig" Data..=)
              Prelude.<$> incrementalPullConfig,
            Prelude.Just ("ConnectorType" Data..= connectorType),
            Prelude.Just
              ( "SourceConnectorProperties"
                  Data..= sourceConnectorProperties
              )
          ]
      )
