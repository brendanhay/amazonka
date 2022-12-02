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
-- Module      : Amazonka.RDS.Types.DBClusterEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBClusterEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This data type represents the information you need to connect to an
-- Amazon Aurora DB cluster. This data type is used as a response element
-- in the following actions:
--
-- -   @CreateDBClusterEndpoint@
--
-- -   @DescribeDBClusterEndpoints@
--
-- -   @ModifyDBClusterEndpoint@
--
-- -   @DeleteDBClusterEndpoint@
--
-- For the data structure that represents Amazon RDS DB instance endpoints,
-- see @Endpoint@.
--
-- /See:/ 'newDBClusterEndpoint' smart constructor.
data DBClusterEndpoint = DBClusterEndpoint'
  { -- | A unique system-generated identifier for an endpoint. It remains the
    -- same for the whole life of the endpoint.
    dbClusterEndpointResourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Prelude.Maybe [Prelude.Text],
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Prelude.Maybe [Prelude.Text],
    -- | The type associated with a custom endpoint. One of: @READER@, @WRITER@,
    -- @ANY@.
    customEndpointType :: Prelude.Maybe Prelude.Text,
    -- | The current status of the endpoint. One of: @creating@, @available@,
    -- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
    -- endpoint that can\'t be used for a certain kind of cluster, such as a
    -- @writer@ endpoint for a read-only secondary cluster in a global
    -- database.
    status :: Prelude.Maybe Prelude.Text,
    -- | The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
    endpointType :: Prelude.Maybe Prelude.Text,
    -- | The identifier associated with the endpoint. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the endpoint.
    dbClusterEndpointArn :: Prelude.Maybe Prelude.Text,
    -- | The DNS address of the endpoint.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterEndpointResourceIdentifier', 'dbClusterEndpoint_dbClusterEndpointResourceIdentifier' - A unique system-generated identifier for an endpoint. It remains the
-- same for the whole life of the endpoint.
--
-- 'staticMembers', 'dbClusterEndpoint_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
--
-- 'dbClusterIdentifier', 'dbClusterEndpoint_dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
--
-- 'excludedMembers', 'dbClusterEndpoint_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
--
-- 'customEndpointType', 'dbClusterEndpoint_customEndpointType' - The type associated with a custom endpoint. One of: @READER@, @WRITER@,
-- @ANY@.
--
-- 'status', 'dbClusterEndpoint_status' - The current status of the endpoint. One of: @creating@, @available@,
-- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
-- endpoint that can\'t be used for a certain kind of cluster, such as a
-- @writer@ endpoint for a read-only secondary cluster in a global
-- database.
--
-- 'endpointType', 'dbClusterEndpoint_endpointType' - The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
--
-- 'dbClusterEndpointIdentifier', 'dbClusterEndpoint_dbClusterEndpointIdentifier' - The identifier associated with the endpoint. This parameter is stored as
-- a lowercase string.
--
-- 'dbClusterEndpointArn', 'dbClusterEndpoint_dbClusterEndpointArn' - The Amazon Resource Name (ARN) for the endpoint.
--
-- 'endpoint', 'dbClusterEndpoint_endpoint' - The DNS address of the endpoint.
newDBClusterEndpoint ::
  DBClusterEndpoint
newDBClusterEndpoint =
  DBClusterEndpoint'
    { dbClusterEndpointResourceIdentifier =
        Prelude.Nothing,
      staticMembers = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      excludedMembers = Prelude.Nothing,
      customEndpointType = Prelude.Nothing,
      status = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      dbClusterEndpointIdentifier = Prelude.Nothing,
      dbClusterEndpointArn = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | A unique system-generated identifier for an endpoint. It remains the
-- same for the whole life of the endpoint.
dbClusterEndpoint_dbClusterEndpointResourceIdentifier :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_dbClusterEndpointResourceIdentifier = Lens.lens (\DBClusterEndpoint' {dbClusterEndpointResourceIdentifier} -> dbClusterEndpointResourceIdentifier) (\s@DBClusterEndpoint' {} a -> s {dbClusterEndpointResourceIdentifier = a} :: DBClusterEndpoint)

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
dbClusterEndpoint_staticMembers :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe [Prelude.Text])
dbClusterEndpoint_staticMembers = Lens.lens (\DBClusterEndpoint' {staticMembers} -> staticMembers) (\s@DBClusterEndpoint' {} a -> s {staticMembers = a} :: DBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
dbClusterEndpoint_dbClusterIdentifier :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_dbClusterIdentifier = Lens.lens (\DBClusterEndpoint' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBClusterEndpoint' {} a -> s {dbClusterIdentifier = a} :: DBClusterEndpoint)

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
dbClusterEndpoint_excludedMembers :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe [Prelude.Text])
dbClusterEndpoint_excludedMembers = Lens.lens (\DBClusterEndpoint' {excludedMembers} -> excludedMembers) (\s@DBClusterEndpoint' {} a -> s {excludedMembers = a} :: DBClusterEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The type associated with a custom endpoint. One of: @READER@, @WRITER@,
-- @ANY@.
dbClusterEndpoint_customEndpointType :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_customEndpointType = Lens.lens (\DBClusterEndpoint' {customEndpointType} -> customEndpointType) (\s@DBClusterEndpoint' {} a -> s {customEndpointType = a} :: DBClusterEndpoint)

-- | The current status of the endpoint. One of: @creating@, @available@,
-- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
-- endpoint that can\'t be used for a certain kind of cluster, such as a
-- @writer@ endpoint for a read-only secondary cluster in a global
-- database.
dbClusterEndpoint_status :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_status = Lens.lens (\DBClusterEndpoint' {status} -> status) (\s@DBClusterEndpoint' {} a -> s {status = a} :: DBClusterEndpoint)

-- | The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
dbClusterEndpoint_endpointType :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_endpointType = Lens.lens (\DBClusterEndpoint' {endpointType} -> endpointType) (\s@DBClusterEndpoint' {} a -> s {endpointType = a} :: DBClusterEndpoint)

-- | The identifier associated with the endpoint. This parameter is stored as
-- a lowercase string.
dbClusterEndpoint_dbClusterEndpointIdentifier :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_dbClusterEndpointIdentifier = Lens.lens (\DBClusterEndpoint' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@DBClusterEndpoint' {} a -> s {dbClusterEndpointIdentifier = a} :: DBClusterEndpoint)

-- | The Amazon Resource Name (ARN) for the endpoint.
dbClusterEndpoint_dbClusterEndpointArn :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_dbClusterEndpointArn = Lens.lens (\DBClusterEndpoint' {dbClusterEndpointArn} -> dbClusterEndpointArn) (\s@DBClusterEndpoint' {} a -> s {dbClusterEndpointArn = a} :: DBClusterEndpoint)

-- | The DNS address of the endpoint.
dbClusterEndpoint_endpoint :: Lens.Lens' DBClusterEndpoint (Prelude.Maybe Prelude.Text)
dbClusterEndpoint_endpoint = Lens.lens (\DBClusterEndpoint' {endpoint} -> endpoint) (\s@DBClusterEndpoint' {} a -> s {endpoint = a} :: DBClusterEndpoint)

instance Data.FromXML DBClusterEndpoint where
  parseXML x =
    DBClusterEndpoint'
      Prelude.<$> (x Data..@? "DBClusterEndpointResourceIdentifier")
      Prelude.<*> ( x Data..@? "StaticMembers" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "DBClusterIdentifier")
      Prelude.<*> ( x Data..@? "ExcludedMembers" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "CustomEndpointType")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "EndpointType")
      Prelude.<*> (x Data..@? "DBClusterEndpointIdentifier")
      Prelude.<*> (x Data..@? "DBClusterEndpointArn")
      Prelude.<*> (x Data..@? "Endpoint")

instance Prelude.Hashable DBClusterEndpoint where
  hashWithSalt _salt DBClusterEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterEndpointResourceIdentifier
      `Prelude.hashWithSalt` staticMembers
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` excludedMembers
      `Prelude.hashWithSalt` customEndpointType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` dbClusterEndpointIdentifier
      `Prelude.hashWithSalt` dbClusterEndpointArn
      `Prelude.hashWithSalt` endpoint

instance Prelude.NFData DBClusterEndpoint where
  rnf DBClusterEndpoint' {..} =
    Prelude.rnf dbClusterEndpointResourceIdentifier
      `Prelude.seq` Prelude.rnf staticMembers
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf excludedMembers
      `Prelude.seq` Prelude.rnf customEndpointType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf dbClusterEndpointIdentifier
      `Prelude.seq` Prelude.rnf dbClusterEndpointArn
      `Prelude.seq` Prelude.rnf endpoint
