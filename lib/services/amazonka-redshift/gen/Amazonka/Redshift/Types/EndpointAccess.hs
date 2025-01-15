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
-- Module      : Amazonka.Redshift.Types.EndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EndpointAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.VpcEndpoint
import Amazonka.Redshift.Types.VpcSecurityGroupMembership

-- | Describes a Redshift-managed VPC endpoint.
--
-- /See:/ 'newEndpointAccess' smart constructor.
data EndpointAccess = EndpointAccess'
  { -- | The DNS address of the endpoint.
    address :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier of the cluster associated with the endpoint.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The time (UTC) that the endpoint was created.
    endpointCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the endpoint.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The status of the endpoint.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the cluster accepts incoming connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account ID of the owner of the cluster.
    resourceOwner :: Prelude.Maybe Prelude.Text,
    -- | The subnet group name where Amazon Redshift chooses to deploy the
    -- endpoint.
    subnetGroupName :: Prelude.Maybe Prelude.Text,
    vpcEndpoint :: Prelude.Maybe VpcEndpoint,
    -- | The security groups associated with the endpoint.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpointAccess_address' - The DNS address of the endpoint.
--
-- 'clusterIdentifier', 'endpointAccess_clusterIdentifier' - The cluster identifier of the cluster associated with the endpoint.
--
-- 'endpointCreateTime', 'endpointAccess_endpointCreateTime' - The time (UTC) that the endpoint was created.
--
-- 'endpointName', 'endpointAccess_endpointName' - The name of the endpoint.
--
-- 'endpointStatus', 'endpointAccess_endpointStatus' - The status of the endpoint.
--
-- 'port', 'endpointAccess_port' - The port number on which the cluster accepts incoming connections.
--
-- 'resourceOwner', 'endpointAccess_resourceOwner' - The Amazon Web Services account ID of the owner of the cluster.
--
-- 'subnetGroupName', 'endpointAccess_subnetGroupName' - The subnet group name where Amazon Redshift chooses to deploy the
-- endpoint.
--
-- 'vpcEndpoint', 'endpointAccess_vpcEndpoint' - Undocumented member.
--
-- 'vpcSecurityGroups', 'endpointAccess_vpcSecurityGroups' - The security groups associated with the endpoint.
newEndpointAccess ::
  EndpointAccess
newEndpointAccess =
  EndpointAccess'
    { address = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      endpointCreateTime = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      port = Prelude.Nothing,
      resourceOwner = Prelude.Nothing,
      subnetGroupName = Prelude.Nothing,
      vpcEndpoint = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | The DNS address of the endpoint.
endpointAccess_address :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_address = Lens.lens (\EndpointAccess' {address} -> address) (\s@EndpointAccess' {} a -> s {address = a} :: EndpointAccess)

-- | The cluster identifier of the cluster associated with the endpoint.
endpointAccess_clusterIdentifier :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_clusterIdentifier = Lens.lens (\EndpointAccess' {clusterIdentifier} -> clusterIdentifier) (\s@EndpointAccess' {} a -> s {clusterIdentifier = a} :: EndpointAccess)

-- | The time (UTC) that the endpoint was created.
endpointAccess_endpointCreateTime :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.UTCTime)
endpointAccess_endpointCreateTime = Lens.lens (\EndpointAccess' {endpointCreateTime} -> endpointCreateTime) (\s@EndpointAccess' {} a -> s {endpointCreateTime = a} :: EndpointAccess) Prelude.. Lens.mapping Data._Time

-- | The name of the endpoint.
endpointAccess_endpointName :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointName = Lens.lens (\EndpointAccess' {endpointName} -> endpointName) (\s@EndpointAccess' {} a -> s {endpointName = a} :: EndpointAccess)

-- | The status of the endpoint.
endpointAccess_endpointStatus :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointStatus = Lens.lens (\EndpointAccess' {endpointStatus} -> endpointStatus) (\s@EndpointAccess' {} a -> s {endpointStatus = a} :: EndpointAccess)

-- | The port number on which the cluster accepts incoming connections.
endpointAccess_port :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Int)
endpointAccess_port = Lens.lens (\EndpointAccess' {port} -> port) (\s@EndpointAccess' {} a -> s {port = a} :: EndpointAccess)

-- | The Amazon Web Services account ID of the owner of the cluster.
endpointAccess_resourceOwner :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_resourceOwner = Lens.lens (\EndpointAccess' {resourceOwner} -> resourceOwner) (\s@EndpointAccess' {} a -> s {resourceOwner = a} :: EndpointAccess)

-- | The subnet group name where Amazon Redshift chooses to deploy the
-- endpoint.
endpointAccess_subnetGroupName :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_subnetGroupName = Lens.lens (\EndpointAccess' {subnetGroupName} -> subnetGroupName) (\s@EndpointAccess' {} a -> s {subnetGroupName = a} :: EndpointAccess)

-- | Undocumented member.
endpointAccess_vpcEndpoint :: Lens.Lens' EndpointAccess (Prelude.Maybe VpcEndpoint)
endpointAccess_vpcEndpoint = Lens.lens (\EndpointAccess' {vpcEndpoint} -> vpcEndpoint) (\s@EndpointAccess' {} a -> s {vpcEndpoint = a} :: EndpointAccess)

-- | The security groups associated with the endpoint.
endpointAccess_vpcSecurityGroups :: Lens.Lens' EndpointAccess (Prelude.Maybe [VpcSecurityGroupMembership])
endpointAccess_vpcSecurityGroups = Lens.lens (\EndpointAccess' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@EndpointAccess' {} a -> s {vpcSecurityGroups = a} :: EndpointAccess) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML EndpointAccess where
  parseXML x =
    EndpointAccess'
      Prelude.<$> (x Data..@? "Address")
      Prelude.<*> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "EndpointCreateTime")
      Prelude.<*> (x Data..@? "EndpointName")
      Prelude.<*> (x Data..@? "EndpointStatus")
      Prelude.<*> (x Data..@? "Port")
      Prelude.<*> (x Data..@? "ResourceOwner")
      Prelude.<*> (x Data..@? "SubnetGroupName")
      Prelude.<*> (x Data..@? "VpcEndpoint")
      Prelude.<*> ( x
                      Data..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "VpcSecurityGroup")
                  )

instance Prelude.Hashable EndpointAccess where
  hashWithSalt _salt EndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` endpointCreateTime
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` resourceOwner
      `Prelude.hashWithSalt` subnetGroupName
      `Prelude.hashWithSalt` vpcEndpoint
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData EndpointAccess where
  rnf EndpointAccess' {..} =
    Prelude.rnf address `Prelude.seq`
      Prelude.rnf clusterIdentifier `Prelude.seq`
        Prelude.rnf endpointCreateTime `Prelude.seq`
          Prelude.rnf endpointName `Prelude.seq`
            Prelude.rnf endpointStatus `Prelude.seq`
              Prelude.rnf port `Prelude.seq`
                Prelude.rnf resourceOwner `Prelude.seq`
                  Prelude.rnf subnetGroupName `Prelude.seq`
                    Prelude.rnf vpcEndpoint `Prelude.seq`
                      Prelude.rnf vpcSecurityGroups
