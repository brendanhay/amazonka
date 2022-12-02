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
-- Module      : Amazonka.AppFlow.Types.ConnectorProfileConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfileConfig where

import Amazonka.AppFlow.Types.ConnectorProfileCredentials
import Amazonka.AppFlow.Types.ConnectorProfileProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the connector-specific configuration and credentials for the
-- connector profile.
--
-- /See:/ 'newConnectorProfileConfig' smart constructor.
data ConnectorProfileConfig = ConnectorProfileConfig'
  { -- | The connector-specific credentials required by each connector.
    connectorProfileCredentials :: Prelude.Maybe ConnectorProfileCredentials,
    -- | The connector-specific properties of the profile configuration.
    connectorProfileProperties :: ConnectorProfileProperties
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
-- 'connectorProfileCredentials', 'connectorProfileConfig_connectorProfileCredentials' - The connector-specific credentials required by each connector.
--
-- 'connectorProfileProperties', 'connectorProfileConfig_connectorProfileProperties' - The connector-specific properties of the profile configuration.
newConnectorProfileConfig ::
  -- | 'connectorProfileProperties'
  ConnectorProfileProperties ->
  ConnectorProfileConfig
newConnectorProfileConfig
  pConnectorProfileProperties_ =
    ConnectorProfileConfig'
      { connectorProfileCredentials =
          Prelude.Nothing,
        connectorProfileProperties =
          pConnectorProfileProperties_
      }

-- | The connector-specific credentials required by each connector.
connectorProfileConfig_connectorProfileCredentials :: Lens.Lens' ConnectorProfileConfig (Prelude.Maybe ConnectorProfileCredentials)
connectorProfileConfig_connectorProfileCredentials = Lens.lens (\ConnectorProfileConfig' {connectorProfileCredentials} -> connectorProfileCredentials) (\s@ConnectorProfileConfig' {} a -> s {connectorProfileCredentials = a} :: ConnectorProfileConfig)

-- | The connector-specific properties of the profile configuration.
connectorProfileConfig_connectorProfileProperties :: Lens.Lens' ConnectorProfileConfig ConnectorProfileProperties
connectorProfileConfig_connectorProfileProperties = Lens.lens (\ConnectorProfileConfig' {connectorProfileProperties} -> connectorProfileProperties) (\s@ConnectorProfileConfig' {} a -> s {connectorProfileProperties = a} :: ConnectorProfileConfig)

instance Prelude.Hashable ConnectorProfileConfig where
  hashWithSalt _salt ConnectorProfileConfig' {..} =
    _salt
      `Prelude.hashWithSalt` connectorProfileCredentials
      `Prelude.hashWithSalt` connectorProfileProperties

instance Prelude.NFData ConnectorProfileConfig where
  rnf ConnectorProfileConfig' {..} =
    Prelude.rnf connectorProfileCredentials
      `Prelude.seq` Prelude.rnf connectorProfileProperties

instance Data.ToJSON ConnectorProfileConfig where
  toJSON ConnectorProfileConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("connectorProfileCredentials" Data..=)
              Prelude.<$> connectorProfileCredentials,
            Prelude.Just
              ( "connectorProfileProperties"
                  Data..= connectorProfileProperties
              )
          ]
      )
