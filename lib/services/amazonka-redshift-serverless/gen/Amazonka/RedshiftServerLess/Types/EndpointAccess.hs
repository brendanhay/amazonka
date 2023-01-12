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
-- Module      : Amazonka.RedshiftServerLess.Types.EndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.EndpointAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.VpcEndpoint
import Amazonka.RedshiftServerLess.Types.VpcSecurityGroupMembership

-- | Information about an Amazon Redshift Serverless VPC endpoint.
--
-- /See:/ 'newEndpointAccess' smart constructor.
data EndpointAccess = EndpointAccess'
  { -- | The DNS address of the endpoint.
    address :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the endpoint was created.
    endpointCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the VPC endpoint.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC endpoint.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | The port number on which Amazon Redshift Serverless accepts incoming
    -- connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of subnets where Amazon Redshift Serverless choose
    -- to deploy the VPC endpoint.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The connection endpoint for connecting to Amazon Redshift Serverless.
    vpcEndpoint :: Prelude.Maybe VpcEndpoint,
    -- | The security groups associated with the endpoint.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | The name of the workgroup associated with the endpoint.
    workgroupName :: Prelude.Maybe Prelude.Text
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
-- 'endpointArn', 'endpointAccess_endpointArn' - The Amazon Resource Name (ARN) of the VPC endpoint.
--
-- 'endpointCreateTime', 'endpointAccess_endpointCreateTime' - The time that the endpoint was created.
--
-- 'endpointName', 'endpointAccess_endpointName' - The name of the VPC endpoint.
--
-- 'endpointStatus', 'endpointAccess_endpointStatus' - The status of the VPC endpoint.
--
-- 'port', 'endpointAccess_port' - The port number on which Amazon Redshift Serverless accepts incoming
-- connections.
--
-- 'subnetIds', 'endpointAccess_subnetIds' - The unique identifier of subnets where Amazon Redshift Serverless choose
-- to deploy the VPC endpoint.
--
-- 'vpcEndpoint', 'endpointAccess_vpcEndpoint' - The connection endpoint for connecting to Amazon Redshift Serverless.
--
-- 'vpcSecurityGroups', 'endpointAccess_vpcSecurityGroups' - The security groups associated with the endpoint.
--
-- 'workgroupName', 'endpointAccess_workgroupName' - The name of the workgroup associated with the endpoint.
newEndpointAccess ::
  EndpointAccess
newEndpointAccess =
  EndpointAccess'
    { address = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      endpointCreateTime = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      port = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcEndpoint = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      workgroupName = Prelude.Nothing
    }

-- | The DNS address of the endpoint.
endpointAccess_address :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_address = Lens.lens (\EndpointAccess' {address} -> address) (\s@EndpointAccess' {} a -> s {address = a} :: EndpointAccess)

-- | The Amazon Resource Name (ARN) of the VPC endpoint.
endpointAccess_endpointArn :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointArn = Lens.lens (\EndpointAccess' {endpointArn} -> endpointArn) (\s@EndpointAccess' {} a -> s {endpointArn = a} :: EndpointAccess)

-- | The time that the endpoint was created.
endpointAccess_endpointCreateTime :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.UTCTime)
endpointAccess_endpointCreateTime = Lens.lens (\EndpointAccess' {endpointCreateTime} -> endpointCreateTime) (\s@EndpointAccess' {} a -> s {endpointCreateTime = a} :: EndpointAccess) Prelude.. Lens.mapping Data._Time

-- | The name of the VPC endpoint.
endpointAccess_endpointName :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointName = Lens.lens (\EndpointAccess' {endpointName} -> endpointName) (\s@EndpointAccess' {} a -> s {endpointName = a} :: EndpointAccess)

-- | The status of the VPC endpoint.
endpointAccess_endpointStatus :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointStatus = Lens.lens (\EndpointAccess' {endpointStatus} -> endpointStatus) (\s@EndpointAccess' {} a -> s {endpointStatus = a} :: EndpointAccess)

-- | The port number on which Amazon Redshift Serverless accepts incoming
-- connections.
endpointAccess_port :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Int)
endpointAccess_port = Lens.lens (\EndpointAccess' {port} -> port) (\s@EndpointAccess' {} a -> s {port = a} :: EndpointAccess)

-- | The unique identifier of subnets where Amazon Redshift Serverless choose
-- to deploy the VPC endpoint.
endpointAccess_subnetIds :: Lens.Lens' EndpointAccess (Prelude.Maybe [Prelude.Text])
endpointAccess_subnetIds = Lens.lens (\EndpointAccess' {subnetIds} -> subnetIds) (\s@EndpointAccess' {} a -> s {subnetIds = a} :: EndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The connection endpoint for connecting to Amazon Redshift Serverless.
endpointAccess_vpcEndpoint :: Lens.Lens' EndpointAccess (Prelude.Maybe VpcEndpoint)
endpointAccess_vpcEndpoint = Lens.lens (\EndpointAccess' {vpcEndpoint} -> vpcEndpoint) (\s@EndpointAccess' {} a -> s {vpcEndpoint = a} :: EndpointAccess)

-- | The security groups associated with the endpoint.
endpointAccess_vpcSecurityGroups :: Lens.Lens' EndpointAccess (Prelude.Maybe [VpcSecurityGroupMembership])
endpointAccess_vpcSecurityGroups = Lens.lens (\EndpointAccess' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@EndpointAccess' {} a -> s {vpcSecurityGroups = a} :: EndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The name of the workgroup associated with the endpoint.
endpointAccess_workgroupName :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_workgroupName = Lens.lens (\EndpointAccess' {workgroupName} -> workgroupName) (\s@EndpointAccess' {} a -> s {workgroupName = a} :: EndpointAccess)

instance Data.FromJSON EndpointAccess where
  parseJSON =
    Data.withObject
      "EndpointAccess"
      ( \x ->
          EndpointAccess'
            Prelude.<$> (x Data..:? "address")
            Prelude.<*> (x Data..:? "endpointArn")
            Prelude.<*> (x Data..:? "endpointCreateTime")
            Prelude.<*> (x Data..:? "endpointName")
            Prelude.<*> (x Data..:? "endpointStatus")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcEndpoint")
            Prelude.<*> ( x Data..:? "vpcSecurityGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "workgroupName")
      )

instance Prelude.Hashable EndpointAccess where
  hashWithSalt _salt EndpointAccess' {..} =
    _salt `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` endpointCreateTime
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcEndpoint
      `Prelude.hashWithSalt` vpcSecurityGroups
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData EndpointAccess where
  rnf EndpointAccess' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf endpointCreateTime
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcEndpoint
      `Prelude.seq` Prelude.rnf vpcSecurityGroups
      `Prelude.seq` Prelude.rnf workgroupName
