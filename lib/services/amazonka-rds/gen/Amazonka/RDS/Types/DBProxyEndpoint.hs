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
-- Module      : Amazonka.RDS.Types.DBProxyEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBProxyEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DBProxyEndpointStatus
import Amazonka.RDS.Types.DBProxyEndpointTargetRole

-- | The data structure representing an endpoint associated with a DB proxy.
-- RDS automatically creates one endpoint for each DB proxy. For Aurora DB
-- clusters, you can associate additional endpoints with the same DB proxy.
-- These endpoints can be read\/write or read-only. They can also reside in
-- different VPCs than the associated DB proxy.
--
-- This data type is used as a response element in the
-- @DescribeDBProxyEndpoints@ operation.
--
-- /See:/ 'newDBProxyEndpoint' smart constructor.
data DBProxyEndpoint = DBProxyEndpoint'
  { -- | The date and time when the DB proxy endpoint was first created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) for the DB proxy endpoint.
    dbProxyEndpointArn :: Prelude.Maybe Prelude.Text,
    -- | The name for the DB proxy endpoint. An identifier must begin with a
    -- letter and must contain only ASCII letters, digits, and hyphens; it
    -- can\'t end with a hyphen or contain two consecutive hyphens.
    dbProxyEndpointName :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the DB proxy that is associated with this DB proxy
    -- endpoint.
    dbProxyName :: Prelude.Maybe Prelude.Text,
    -- | The endpoint that you can use to connect to the DB proxy. You include
    -- the endpoint value in the connection string for a database client
    -- application.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether this endpoint is the default endpoint for
    -- the associated DB proxy. Default DB proxy endpoints always have
    -- read\/write capability. Other endpoints that you associate with the DB
    -- proxy can be either read\/write or read-only.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The current status of this DB proxy endpoint. A status of @available@
    -- means the endpoint is ready to handle requests. Other values indicate
    -- that you must wait for the endpoint to be ready, or take some action to
    -- resolve an issue.
    status :: Prelude.Maybe DBProxyEndpointStatus,
    -- | A value that indicates whether the DB proxy endpoint can be used for
    -- read\/write or read-only operations.
    targetRole :: Prelude.Maybe DBProxyEndpointTargetRole,
    -- | Provides the VPC ID of the DB proxy endpoint.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of VPC security groups that the DB proxy endpoint
    -- belongs to.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 subnet IDs for the DB proxy endpoint.
    vpcSubnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBProxyEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'dbProxyEndpoint_createdDate' - The date and time when the DB proxy endpoint was first created.
--
-- 'dbProxyEndpointArn', 'dbProxyEndpoint_dbProxyEndpointArn' - The Amazon Resource Name (ARN) for the DB proxy endpoint.
--
-- 'dbProxyEndpointName', 'dbProxyEndpoint_dbProxyEndpointName' - The name for the DB proxy endpoint. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'dbProxyName', 'dbProxyEndpoint_dbProxyName' - The identifier for the DB proxy that is associated with this DB proxy
-- endpoint.
--
-- 'endpoint', 'dbProxyEndpoint_endpoint' - The endpoint that you can use to connect to the DB proxy. You include
-- the endpoint value in the connection string for a database client
-- application.
--
-- 'isDefault', 'dbProxyEndpoint_isDefault' - A value that indicates whether this endpoint is the default endpoint for
-- the associated DB proxy. Default DB proxy endpoints always have
-- read\/write capability. Other endpoints that you associate with the DB
-- proxy can be either read\/write or read-only.
--
-- 'status', 'dbProxyEndpoint_status' - The current status of this DB proxy endpoint. A status of @available@
-- means the endpoint is ready to handle requests. Other values indicate
-- that you must wait for the endpoint to be ready, or take some action to
-- resolve an issue.
--
-- 'targetRole', 'dbProxyEndpoint_targetRole' - A value that indicates whether the DB proxy endpoint can be used for
-- read\/write or read-only operations.
--
-- 'vpcId', 'dbProxyEndpoint_vpcId' - Provides the VPC ID of the DB proxy endpoint.
--
-- 'vpcSecurityGroupIds', 'dbProxyEndpoint_vpcSecurityGroupIds' - Provides a list of VPC security groups that the DB proxy endpoint
-- belongs to.
--
-- 'vpcSubnetIds', 'dbProxyEndpoint_vpcSubnetIds' - The EC2 subnet IDs for the DB proxy endpoint.
newDBProxyEndpoint ::
  DBProxyEndpoint
newDBProxyEndpoint =
  DBProxyEndpoint'
    { createdDate = Prelude.Nothing,
      dbProxyEndpointArn = Prelude.Nothing,
      dbProxyEndpointName = Prelude.Nothing,
      dbProxyName = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      status = Prelude.Nothing,
      targetRole = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      vpcSubnetIds = Prelude.Nothing
    }

-- | The date and time when the DB proxy endpoint was first created.
dbProxyEndpoint_createdDate :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.UTCTime)
dbProxyEndpoint_createdDate = Lens.lens (\DBProxyEndpoint' {createdDate} -> createdDate) (\s@DBProxyEndpoint' {} a -> s {createdDate = a} :: DBProxyEndpoint) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the DB proxy endpoint.
dbProxyEndpoint_dbProxyEndpointArn :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.Text)
dbProxyEndpoint_dbProxyEndpointArn = Lens.lens (\DBProxyEndpoint' {dbProxyEndpointArn} -> dbProxyEndpointArn) (\s@DBProxyEndpoint' {} a -> s {dbProxyEndpointArn = a} :: DBProxyEndpoint)

-- | The name for the DB proxy endpoint. An identifier must begin with a
-- letter and must contain only ASCII letters, digits, and hyphens; it
-- can\'t end with a hyphen or contain two consecutive hyphens.
dbProxyEndpoint_dbProxyEndpointName :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.Text)
dbProxyEndpoint_dbProxyEndpointName = Lens.lens (\DBProxyEndpoint' {dbProxyEndpointName} -> dbProxyEndpointName) (\s@DBProxyEndpoint' {} a -> s {dbProxyEndpointName = a} :: DBProxyEndpoint)

-- | The identifier for the DB proxy that is associated with this DB proxy
-- endpoint.
dbProxyEndpoint_dbProxyName :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.Text)
dbProxyEndpoint_dbProxyName = Lens.lens (\DBProxyEndpoint' {dbProxyName} -> dbProxyName) (\s@DBProxyEndpoint' {} a -> s {dbProxyName = a} :: DBProxyEndpoint)

-- | The endpoint that you can use to connect to the DB proxy. You include
-- the endpoint value in the connection string for a database client
-- application.
dbProxyEndpoint_endpoint :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.Text)
dbProxyEndpoint_endpoint = Lens.lens (\DBProxyEndpoint' {endpoint} -> endpoint) (\s@DBProxyEndpoint' {} a -> s {endpoint = a} :: DBProxyEndpoint)

-- | A value that indicates whether this endpoint is the default endpoint for
-- the associated DB proxy. Default DB proxy endpoints always have
-- read\/write capability. Other endpoints that you associate with the DB
-- proxy can be either read\/write or read-only.
dbProxyEndpoint_isDefault :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.Bool)
dbProxyEndpoint_isDefault = Lens.lens (\DBProxyEndpoint' {isDefault} -> isDefault) (\s@DBProxyEndpoint' {} a -> s {isDefault = a} :: DBProxyEndpoint)

-- | The current status of this DB proxy endpoint. A status of @available@
-- means the endpoint is ready to handle requests. Other values indicate
-- that you must wait for the endpoint to be ready, or take some action to
-- resolve an issue.
dbProxyEndpoint_status :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe DBProxyEndpointStatus)
dbProxyEndpoint_status = Lens.lens (\DBProxyEndpoint' {status} -> status) (\s@DBProxyEndpoint' {} a -> s {status = a} :: DBProxyEndpoint)

-- | A value that indicates whether the DB proxy endpoint can be used for
-- read\/write or read-only operations.
dbProxyEndpoint_targetRole :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe DBProxyEndpointTargetRole)
dbProxyEndpoint_targetRole = Lens.lens (\DBProxyEndpoint' {targetRole} -> targetRole) (\s@DBProxyEndpoint' {} a -> s {targetRole = a} :: DBProxyEndpoint)

-- | Provides the VPC ID of the DB proxy endpoint.
dbProxyEndpoint_vpcId :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe Prelude.Text)
dbProxyEndpoint_vpcId = Lens.lens (\DBProxyEndpoint' {vpcId} -> vpcId) (\s@DBProxyEndpoint' {} a -> s {vpcId = a} :: DBProxyEndpoint)

-- | Provides a list of VPC security groups that the DB proxy endpoint
-- belongs to.
dbProxyEndpoint_vpcSecurityGroupIds :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe [Prelude.Text])
dbProxyEndpoint_vpcSecurityGroupIds = Lens.lens (\DBProxyEndpoint' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@DBProxyEndpoint' {} a -> s {vpcSecurityGroupIds = a} :: DBProxyEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 subnet IDs for the DB proxy endpoint.
dbProxyEndpoint_vpcSubnetIds :: Lens.Lens' DBProxyEndpoint (Prelude.Maybe [Prelude.Text])
dbProxyEndpoint_vpcSubnetIds = Lens.lens (\DBProxyEndpoint' {vpcSubnetIds} -> vpcSubnetIds) (\s@DBProxyEndpoint' {} a -> s {vpcSubnetIds = a} :: DBProxyEndpoint) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DBProxyEndpoint where
  parseXML x =
    DBProxyEndpoint'
      Prelude.<$> (x Data..@? "CreatedDate")
      Prelude.<*> (x Data..@? "DBProxyEndpointArn")
      Prelude.<*> (x Data..@? "DBProxyEndpointName")
      Prelude.<*> (x Data..@? "DBProxyName")
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "IsDefault")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "TargetRole")
      Prelude.<*> (x Data..@? "VpcId")
      Prelude.<*> ( x
                      Data..@? "VpcSecurityGroupIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "VpcSubnetIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable DBProxyEndpoint where
  hashWithSalt _salt DBProxyEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` dbProxyEndpointArn
      `Prelude.hashWithSalt` dbProxyEndpointName
      `Prelude.hashWithSalt` dbProxyName
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetRole
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` vpcSubnetIds

instance Prelude.NFData DBProxyEndpoint where
  rnf DBProxyEndpoint' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf dbProxyEndpointArn
      `Prelude.seq` Prelude.rnf dbProxyEndpointName
      `Prelude.seq` Prelude.rnf dbProxyName
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetRole
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf vpcSubnetIds
