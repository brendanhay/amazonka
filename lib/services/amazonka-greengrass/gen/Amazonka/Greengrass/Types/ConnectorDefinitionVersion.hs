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
-- Module      : Amazonka.Greengrass.Types.ConnectorDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.ConnectorDefinitionVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.Connector
import qualified Amazonka.Prelude as Prelude

-- | Information about the connector definition version, which is a container
-- for connectors.
--
-- /See:/ 'newConnectorDefinitionVersion' smart constructor.
data ConnectorDefinitionVersion = ConnectorDefinitionVersion'
  { -- | A list of references to connectors in this version, with their
    -- corresponding configuration settings.
    connectors :: Prelude.Maybe [Connector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectors', 'connectorDefinitionVersion_connectors' - A list of references to connectors in this version, with their
-- corresponding configuration settings.
newConnectorDefinitionVersion ::
  ConnectorDefinitionVersion
newConnectorDefinitionVersion =
  ConnectorDefinitionVersion'
    { connectors =
        Prelude.Nothing
    }

-- | A list of references to connectors in this version, with their
-- corresponding configuration settings.
connectorDefinitionVersion_connectors :: Lens.Lens' ConnectorDefinitionVersion (Prelude.Maybe [Connector])
connectorDefinitionVersion_connectors = Lens.lens (\ConnectorDefinitionVersion' {connectors} -> connectors) (\s@ConnectorDefinitionVersion' {} a -> s {connectors = a} :: ConnectorDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConnectorDefinitionVersion where
  parseJSON =
    Data.withObject
      "ConnectorDefinitionVersion"
      ( \x ->
          ConnectorDefinitionVersion'
            Prelude.<$> (x Data..:? "Connectors" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ConnectorDefinitionVersion where
  hashWithSalt _salt ConnectorDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` connectors

instance Prelude.NFData ConnectorDefinitionVersion where
  rnf ConnectorDefinitionVersion' {..} =
    Prelude.rnf connectors

instance Data.ToJSON ConnectorDefinitionVersion where
  toJSON ConnectorDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Connectors" Data..=) Prelude.<$> connectors]
      )
