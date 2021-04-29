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
-- Module      : Network.AWS.Glue.Types.ConnectionInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionInput where

import Network.AWS.Glue.Types.ConnectionPropertyKey
import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that is used to specify a connection to create or update.
--
-- /See:/ 'newConnectionInput' smart constructor.
data ConnectionInput = ConnectionInput'
  { -- | A map of physical connection requirements, such as virtual private cloud
    -- (VPC) and @SecurityGroup@, that are needed to successfully make this
    -- connection.
    physicalConnectionRequirements :: Prelude.Maybe PhysicalConnectionRequirements,
    -- | The description of the connection.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of criteria that can be used in selecting this connection.
    matchCriteria :: Prelude.Maybe [Prelude.Text],
    -- | The name of the connection.
    name :: Prelude.Text,
    -- | The type of the connection. Currently, these types are supported:
    --
    -- -   @JDBC@ - Designates a connection to a database through Java Database
    --     Connectivity (JDBC).
    --
    -- -   @KAFKA@ - Designates a connection to an Apache Kafka streaming
    --     platform.
    --
    -- -   @MONGODB@ - Designates a connection to a MongoDB document database.
    --
    -- -   @NETWORK@ - Designates a network connection to a data source within
    --     an Amazon Virtual Private Cloud environment (Amazon VPC).
    --
    -- -   @MARKETPLACE@ - Uses configuration settings contained in a connector
    --     purchased from AWS Marketplace to read from and write to data stores
    --     that are not natively supported by AWS Glue.
    --
    -- -   @CUSTOM@ - Uses configuration settings contained in a custom
    --     connector to read from and write to data stores that are not
    --     natively supported by AWS Glue.
    --
    -- SFTP is not supported.
    connectionType :: ConnectionType,
    -- | These key-value pairs define parameters for the connection.
    connectionProperties :: Prelude.HashMap ConnectionPropertyKey Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalConnectionRequirements', 'connectionInput_physicalConnectionRequirements' - A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to successfully make this
-- connection.
--
-- 'description', 'connectionInput_description' - The description of the connection.
--
-- 'matchCriteria', 'connectionInput_matchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- 'name', 'connectionInput_name' - The name of the connection.
--
-- 'connectionType', 'connectionInput_connectionType' - The type of the connection. Currently, these types are supported:
--
-- -   @JDBC@ - Designates a connection to a database through Java Database
--     Connectivity (JDBC).
--
-- -   @KAFKA@ - Designates a connection to an Apache Kafka streaming
--     platform.
--
-- -   @MONGODB@ - Designates a connection to a MongoDB document database.
--
-- -   @NETWORK@ - Designates a network connection to a data source within
--     an Amazon Virtual Private Cloud environment (Amazon VPC).
--
-- -   @MARKETPLACE@ - Uses configuration settings contained in a connector
--     purchased from AWS Marketplace to read from and write to data stores
--     that are not natively supported by AWS Glue.
--
-- -   @CUSTOM@ - Uses configuration settings contained in a custom
--     connector to read from and write to data stores that are not
--     natively supported by AWS Glue.
--
-- SFTP is not supported.
--
-- 'connectionProperties', 'connectionInput_connectionProperties' - These key-value pairs define parameters for the connection.
newConnectionInput ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionType'
  ConnectionType ->
  ConnectionInput
newConnectionInput pName_ pConnectionType_ =
  ConnectionInput'
    { physicalConnectionRequirements =
        Prelude.Nothing,
      description = Prelude.Nothing,
      matchCriteria = Prelude.Nothing,
      name = pName_,
      connectionType = pConnectionType_,
      connectionProperties = Prelude.mempty
    }

-- | A map of physical connection requirements, such as virtual private cloud
-- (VPC) and @SecurityGroup@, that are needed to successfully make this
-- connection.
connectionInput_physicalConnectionRequirements :: Lens.Lens' ConnectionInput (Prelude.Maybe PhysicalConnectionRequirements)
connectionInput_physicalConnectionRequirements = Lens.lens (\ConnectionInput' {physicalConnectionRequirements} -> physicalConnectionRequirements) (\s@ConnectionInput' {} a -> s {physicalConnectionRequirements = a} :: ConnectionInput)

-- | The description of the connection.
connectionInput_description :: Lens.Lens' ConnectionInput (Prelude.Maybe Prelude.Text)
connectionInput_description = Lens.lens (\ConnectionInput' {description} -> description) (\s@ConnectionInput' {} a -> s {description = a} :: ConnectionInput)

-- | A list of criteria that can be used in selecting this connection.
connectionInput_matchCriteria :: Lens.Lens' ConnectionInput (Prelude.Maybe [Prelude.Text])
connectionInput_matchCriteria = Lens.lens (\ConnectionInput' {matchCriteria} -> matchCriteria) (\s@ConnectionInput' {} a -> s {matchCriteria = a} :: ConnectionInput) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the connection.
connectionInput_name :: Lens.Lens' ConnectionInput Prelude.Text
connectionInput_name = Lens.lens (\ConnectionInput' {name} -> name) (\s@ConnectionInput' {} a -> s {name = a} :: ConnectionInput)

-- | The type of the connection. Currently, these types are supported:
--
-- -   @JDBC@ - Designates a connection to a database through Java Database
--     Connectivity (JDBC).
--
-- -   @KAFKA@ - Designates a connection to an Apache Kafka streaming
--     platform.
--
-- -   @MONGODB@ - Designates a connection to a MongoDB document database.
--
-- -   @NETWORK@ - Designates a network connection to a data source within
--     an Amazon Virtual Private Cloud environment (Amazon VPC).
--
-- -   @MARKETPLACE@ - Uses configuration settings contained in a connector
--     purchased from AWS Marketplace to read from and write to data stores
--     that are not natively supported by AWS Glue.
--
-- -   @CUSTOM@ - Uses configuration settings contained in a custom
--     connector to read from and write to data stores that are not
--     natively supported by AWS Glue.
--
-- SFTP is not supported.
connectionInput_connectionType :: Lens.Lens' ConnectionInput ConnectionType
connectionInput_connectionType = Lens.lens (\ConnectionInput' {connectionType} -> connectionType) (\s@ConnectionInput' {} a -> s {connectionType = a} :: ConnectionInput)

-- | These key-value pairs define parameters for the connection.
connectionInput_connectionProperties :: Lens.Lens' ConnectionInput (Prelude.HashMap ConnectionPropertyKey Prelude.Text)
connectionInput_connectionProperties = Lens.lens (\ConnectionInput' {connectionProperties} -> connectionProperties) (\s@ConnectionInput' {} a -> s {connectionProperties = a} :: ConnectionInput) Prelude.. Prelude._Coerce

instance Prelude.Hashable ConnectionInput

instance Prelude.NFData ConnectionInput

instance Prelude.ToJSON ConnectionInput where
  toJSON ConnectionInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PhysicalConnectionRequirements" Prelude..=)
              Prelude.<$> physicalConnectionRequirements,
            ("Description" Prelude..=) Prelude.<$> description,
            ("MatchCriteria" Prelude..=)
              Prelude.<$> matchCriteria,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("ConnectionType" Prelude..= connectionType),
            Prelude.Just
              ( "ConnectionProperties"
                  Prelude..= connectionProperties
              )
          ]
      )
