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
-- Module      : Amazonka.AppFlow.Types.DestinationFlowConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DestinationFlowConfig where

import Amazonka.AppFlow.Types.ConnectorType
import Amazonka.AppFlow.Types.DestinationConnectorProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the configuration of destination connectors
-- present in the flow.
--
-- /See:/ 'newDestinationFlowConfig' smart constructor.
data DestinationFlowConfig = DestinationFlowConfig'
  { -- | The API version that the destination connector uses.
    apiVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. This name must be unique for each
    -- connector profile in the Amazon Web Services account.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: ConnectorType,
    -- | This stores the information that is required to query a particular
    -- connector.
    destinationConnectorProperties :: DestinationConnectorProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationFlowConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiVersion', 'destinationFlowConfig_apiVersion' - The API version that the destination connector uses.
--
-- 'connectorProfileName', 'destinationFlowConfig_connectorProfileName' - The name of the connector profile. This name must be unique for each
-- connector profile in the Amazon Web Services account.
--
-- 'connectorType', 'destinationFlowConfig_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
--
-- 'destinationConnectorProperties', 'destinationFlowConfig_destinationConnectorProperties' - This stores the information that is required to query a particular
-- connector.
newDestinationFlowConfig ::
  -- | 'connectorType'
  ConnectorType ->
  -- | 'destinationConnectorProperties'
  DestinationConnectorProperties ->
  DestinationFlowConfig
newDestinationFlowConfig
  pConnectorType_
  pDestinationConnectorProperties_ =
    DestinationFlowConfig'
      { apiVersion =
          Prelude.Nothing,
        connectorProfileName = Prelude.Nothing,
        connectorType = pConnectorType_,
        destinationConnectorProperties =
          pDestinationConnectorProperties_
      }

-- | The API version that the destination connector uses.
destinationFlowConfig_apiVersion :: Lens.Lens' DestinationFlowConfig (Prelude.Maybe Prelude.Text)
destinationFlowConfig_apiVersion = Lens.lens (\DestinationFlowConfig' {apiVersion} -> apiVersion) (\s@DestinationFlowConfig' {} a -> s {apiVersion = a} :: DestinationFlowConfig)

-- | The name of the connector profile. This name must be unique for each
-- connector profile in the Amazon Web Services account.
destinationFlowConfig_connectorProfileName :: Lens.Lens' DestinationFlowConfig (Prelude.Maybe Prelude.Text)
destinationFlowConfig_connectorProfileName = Lens.lens (\DestinationFlowConfig' {connectorProfileName} -> connectorProfileName) (\s@DestinationFlowConfig' {} a -> s {connectorProfileName = a} :: DestinationFlowConfig)

-- | The type of connector, such as Salesforce, Amplitude, and so on.
destinationFlowConfig_connectorType :: Lens.Lens' DestinationFlowConfig ConnectorType
destinationFlowConfig_connectorType = Lens.lens (\DestinationFlowConfig' {connectorType} -> connectorType) (\s@DestinationFlowConfig' {} a -> s {connectorType = a} :: DestinationFlowConfig)

-- | This stores the information that is required to query a particular
-- connector.
destinationFlowConfig_destinationConnectorProperties :: Lens.Lens' DestinationFlowConfig DestinationConnectorProperties
destinationFlowConfig_destinationConnectorProperties = Lens.lens (\DestinationFlowConfig' {destinationConnectorProperties} -> destinationConnectorProperties) (\s@DestinationFlowConfig' {} a -> s {destinationConnectorProperties = a} :: DestinationFlowConfig)

instance Core.FromJSON DestinationFlowConfig where
  parseJSON =
    Core.withObject
      "DestinationFlowConfig"
      ( \x ->
          DestinationFlowConfig'
            Prelude.<$> (x Core..:? "apiVersion")
            Prelude.<*> (x Core..:? "connectorProfileName")
            Prelude.<*> (x Core..: "connectorType")
            Prelude.<*> (x Core..: "destinationConnectorProperties")
      )

instance Prelude.Hashable DestinationFlowConfig where
  hashWithSalt _salt DestinationFlowConfig' {..} =
    _salt `Prelude.hashWithSalt` apiVersion
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` destinationConnectorProperties

instance Prelude.NFData DestinationFlowConfig where
  rnf DestinationFlowConfig' {..} =
    Prelude.rnf apiVersion
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf destinationConnectorProperties

instance Core.ToJSON DestinationFlowConfig where
  toJSON DestinationFlowConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("apiVersion" Core..=) Prelude.<$> apiVersion,
            ("connectorProfileName" Core..=)
              Prelude.<$> connectorProfileName,
            Prelude.Just ("connectorType" Core..= connectorType),
            Prelude.Just
              ( "destinationConnectorProperties"
                  Core..= destinationConnectorProperties
              )
          ]
      )
