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
-- Module      : Network.AWS.RDS.Types.DBClusterEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
  { -- | The identifier associated with the endpoint. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Core.Maybe Core.Text,
    -- | The current status of the endpoint. One of: @creating@, @available@,
    -- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
    -- endpoint that can\'t be used for a certain kind of cluster, such as a
    -- @writer@ endpoint for a read-only secondary cluster in a global
    -- database.
    status :: Core.Maybe Core.Text,
    -- | List of DB instance identifiers that aren\'t part of the custom endpoint
    -- group. All other eligible instances are reachable through the custom
    -- endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Core.Maybe [Core.Text],
    -- | The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
    endpointType :: Core.Maybe Core.Text,
    -- | The type associated with a custom endpoint. One of: @READER@, @WRITER@,
    -- @ANY@.
    customEndpointType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the endpoint.
    dbClusterEndpointArn :: Core.Maybe Core.Text,
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | A unique system-generated identifier for an endpoint. It remains the
    -- same for the whole life of the endpoint.
    dbClusterEndpointResourceIdentifier :: Core.Maybe Core.Text,
    -- | The DNS address of the endpoint.
    endpoint :: Core.Maybe Core.Text,
    -- | List of DB instance identifiers that are part of the custom endpoint
    -- group.
    staticMembers :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DBClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterEndpointIdentifier', 'dbClusterEndpoint_dbClusterEndpointIdentifier' - The identifier associated with the endpoint. This parameter is stored as
-- a lowercase string.
--
-- 'status', 'dbClusterEndpoint_status' - The current status of the endpoint. One of: @creating@, @available@,
-- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
-- endpoint that can\'t be used for a certain kind of cluster, such as a
-- @writer@ endpoint for a read-only secondary cluster in a global
-- database.
--
-- 'excludedMembers', 'dbClusterEndpoint_excludedMembers' - List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
--
-- 'endpointType', 'dbClusterEndpoint_endpointType' - The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
--
-- 'customEndpointType', 'dbClusterEndpoint_customEndpointType' - The type associated with a custom endpoint. One of: @READER@, @WRITER@,
-- @ANY@.
--
-- 'dbClusterEndpointArn', 'dbClusterEndpoint_dbClusterEndpointArn' - The Amazon Resource Name (ARN) for the endpoint.
--
-- 'dbClusterIdentifier', 'dbClusterEndpoint_dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
--
-- 'dbClusterEndpointResourceIdentifier', 'dbClusterEndpoint_dbClusterEndpointResourceIdentifier' - A unique system-generated identifier for an endpoint. It remains the
-- same for the whole life of the endpoint.
--
-- 'endpoint', 'dbClusterEndpoint_endpoint' - The DNS address of the endpoint.
--
-- 'staticMembers', 'dbClusterEndpoint_staticMembers' - List of DB instance identifiers that are part of the custom endpoint
-- group.
newDBClusterEndpoint ::
  DBClusterEndpoint
newDBClusterEndpoint =
  DBClusterEndpoint'
    { dbClusterEndpointIdentifier =
        Core.Nothing,
      status = Core.Nothing,
      excludedMembers = Core.Nothing,
      endpointType = Core.Nothing,
      customEndpointType = Core.Nothing,
      dbClusterEndpointArn = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      dbClusterEndpointResourceIdentifier = Core.Nothing,
      endpoint = Core.Nothing,
      staticMembers = Core.Nothing
    }

-- | The identifier associated with the endpoint. This parameter is stored as
-- a lowercase string.
dbClusterEndpoint_dbClusterEndpointIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_dbClusterEndpointIdentifier = Lens.lens (\DBClusterEndpoint' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@DBClusterEndpoint' {} a -> s {dbClusterEndpointIdentifier = a} :: DBClusterEndpoint)

-- | The current status of the endpoint. One of: @creating@, @available@,
-- @deleting@, @inactive@, @modifying@. The @inactive@ state applies to an
-- endpoint that can\'t be used for a certain kind of cluster, such as a
-- @writer@ endpoint for a read-only secondary cluster in a global
-- database.
dbClusterEndpoint_status :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_status = Lens.lens (\DBClusterEndpoint' {status} -> status) (\s@DBClusterEndpoint' {} a -> s {status = a} :: DBClusterEndpoint)

-- | List of DB instance identifiers that aren\'t part of the custom endpoint
-- group. All other eligible instances are reachable through the custom
-- endpoint. Only relevant if the list of static members is empty.
dbClusterEndpoint_excludedMembers :: Lens.Lens' DBClusterEndpoint (Core.Maybe [Core.Text])
dbClusterEndpoint_excludedMembers = Lens.lens (\DBClusterEndpoint' {excludedMembers} -> excludedMembers) (\s@DBClusterEndpoint' {} a -> s {excludedMembers = a} :: DBClusterEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The type of the endpoint. One of: @READER@, @WRITER@, @CUSTOM@.
dbClusterEndpoint_endpointType :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_endpointType = Lens.lens (\DBClusterEndpoint' {endpointType} -> endpointType) (\s@DBClusterEndpoint' {} a -> s {endpointType = a} :: DBClusterEndpoint)

-- | The type associated with a custom endpoint. One of: @READER@, @WRITER@,
-- @ANY@.
dbClusterEndpoint_customEndpointType :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_customEndpointType = Lens.lens (\DBClusterEndpoint' {customEndpointType} -> customEndpointType) (\s@DBClusterEndpoint' {} a -> s {customEndpointType = a} :: DBClusterEndpoint)

-- | The Amazon Resource Name (ARN) for the endpoint.
dbClusterEndpoint_dbClusterEndpointArn :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_dbClusterEndpointArn = Lens.lens (\DBClusterEndpoint' {dbClusterEndpointArn} -> dbClusterEndpointArn) (\s@DBClusterEndpoint' {} a -> s {dbClusterEndpointArn = a} :: DBClusterEndpoint)

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
dbClusterEndpoint_dbClusterIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_dbClusterIdentifier = Lens.lens (\DBClusterEndpoint' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBClusterEndpoint' {} a -> s {dbClusterIdentifier = a} :: DBClusterEndpoint)

-- | A unique system-generated identifier for an endpoint. It remains the
-- same for the whole life of the endpoint.
dbClusterEndpoint_dbClusterEndpointResourceIdentifier :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_dbClusterEndpointResourceIdentifier = Lens.lens (\DBClusterEndpoint' {dbClusterEndpointResourceIdentifier} -> dbClusterEndpointResourceIdentifier) (\s@DBClusterEndpoint' {} a -> s {dbClusterEndpointResourceIdentifier = a} :: DBClusterEndpoint)

-- | The DNS address of the endpoint.
dbClusterEndpoint_endpoint :: Lens.Lens' DBClusterEndpoint (Core.Maybe Core.Text)
dbClusterEndpoint_endpoint = Lens.lens (\DBClusterEndpoint' {endpoint} -> endpoint) (\s@DBClusterEndpoint' {} a -> s {endpoint = a} :: DBClusterEndpoint)

-- | List of DB instance identifiers that are part of the custom endpoint
-- group.
dbClusterEndpoint_staticMembers :: Lens.Lens' DBClusterEndpoint (Core.Maybe [Core.Text])
dbClusterEndpoint_staticMembers = Lens.lens (\DBClusterEndpoint' {staticMembers} -> staticMembers) (\s@DBClusterEndpoint' {} a -> s {staticMembers = a} :: DBClusterEndpoint) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML DBClusterEndpoint where
  parseXML x =
    DBClusterEndpoint'
      Core.<$> (x Core..@? "DBClusterEndpointIdentifier")
      Core.<*> (x Core..@? "Status")
      Core.<*> ( x Core..@? "ExcludedMembers" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "EndpointType")
      Core.<*> (x Core..@? "CustomEndpointType")
      Core.<*> (x Core..@? "DBClusterEndpointArn")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "DBClusterEndpointResourceIdentifier")
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> ( x Core..@? "StaticMembers" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable DBClusterEndpoint

instance Core.NFData DBClusterEndpoint
