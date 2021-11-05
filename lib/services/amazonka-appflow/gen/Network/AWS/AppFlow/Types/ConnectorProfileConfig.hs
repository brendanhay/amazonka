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
-- Module      : Network.AWS.AppFlow.Types.ConnectorProfileConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.ConnectorProfileConfig where

import Network.AWS.AppFlow.Types.ConnectorProfileCredentials
import Network.AWS.AppFlow.Types.ConnectorProfileProperties
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the connector-specific configuration and credentials for the
-- connector profile.
--
-- /See:/ 'newConnectorProfileConfig' smart constructor.
data ConnectorProfileConfig = ConnectorProfileConfig'
  { -- | The connector-specific properties of the profile configuration.
    connectorProfileProperties :: ConnectorProfileProperties,
    -- | The connector-specific credentials required by each connector.
    connectorProfileCredentials :: ConnectorProfileCredentials
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorProfileConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorProfileProperties', 'connectorProfileConfig_connectorProfileProperties' - The connector-specific properties of the profile configuration.
--
-- 'connectorProfileCredentials', 'connectorProfileConfig_connectorProfileCredentials' - The connector-specific credentials required by each connector.
newConnectorProfileConfig ::
  -- | 'connectorProfileProperties'
  ConnectorProfileProperties ->
  -- | 'connectorProfileCredentials'
  ConnectorProfileCredentials ->
  ConnectorProfileConfig
newConnectorProfileConfig
  pConnectorProfileProperties_
  pConnectorProfileCredentials_ =
    ConnectorProfileConfig'
      { connectorProfileProperties =
          pConnectorProfileProperties_,
        connectorProfileCredentials =
          pConnectorProfileCredentials_
      }

-- | The connector-specific properties of the profile configuration.
connectorProfileConfig_connectorProfileProperties :: Lens.Lens' ConnectorProfileConfig ConnectorProfileProperties
connectorProfileConfig_connectorProfileProperties = Lens.lens (\ConnectorProfileConfig' {connectorProfileProperties} -> connectorProfileProperties) (\s@ConnectorProfileConfig' {} a -> s {connectorProfileProperties = a} :: ConnectorProfileConfig)

-- | The connector-specific credentials required by each connector.
connectorProfileConfig_connectorProfileCredentials :: Lens.Lens' ConnectorProfileConfig ConnectorProfileCredentials
connectorProfileConfig_connectorProfileCredentials = Lens.lens (\ConnectorProfileConfig' {connectorProfileCredentials} -> connectorProfileCredentials) (\s@ConnectorProfileConfig' {} a -> s {connectorProfileCredentials = a} :: ConnectorProfileConfig)

instance Prelude.Hashable ConnectorProfileConfig

instance Prelude.NFData ConnectorProfileConfig

instance Core.ToJSON ConnectorProfileConfig where
  toJSON ConnectorProfileConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "connectorProfileProperties"
                  Core..= connectorProfileProperties
              ),
            Prelude.Just
              ( "connectorProfileCredentials"
                  Core..= connectorProfileCredentials
              )
          ]
      )
