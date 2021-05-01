{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectorDefinitionVersion where

import Network.AWS.Greengrass.Types.Connector
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the connector definition version, which is a container
-- for connectors.
--
-- /See:/ 'newConnectorDefinitionVersion' smart constructor.
data ConnectorDefinitionVersion = ConnectorDefinitionVersion'
  { -- | A list of references to connectors in this version, with their
    -- corresponding configuration settings.
    connectors :: Prelude.Maybe [Connector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
connectorDefinitionVersion_connectors = Lens.lens (\ConnectorDefinitionVersion' {connectors} -> connectors) (\s@ConnectorDefinitionVersion' {} a -> s {connectors = a} :: ConnectorDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ConnectorDefinitionVersion where
  parseJSON =
    Prelude.withObject
      "ConnectorDefinitionVersion"
      ( \x ->
          ConnectorDefinitionVersion'
            Prelude.<$> ( x Prelude..:? "Connectors"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConnectorDefinitionVersion

instance Prelude.NFData ConnectorDefinitionVersion

instance Prelude.ToJSON ConnectorDefinitionVersion where
  toJSON ConnectorDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Connectors" Prelude..=) Prelude.<$> connectors]
      )
