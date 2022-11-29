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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.EndpointAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.VpcEndpoint
import Amazonka.RedshiftServerLess.Types.VpcSecurityGroupMembership

-- | Information about an Amazon Redshift Serverless VPC endpoint.
--
-- /See:/ 'newEndpointAccess' smart constructor.
data EndpointAccess = EndpointAccess'
  { -- | The port number on which Amazon Redshift Serverless accepts incoming
    -- connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | The name of the VPC endpoint.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The name of the workgroup associated with the endpoint.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The DNS address of the endpoint.
    address :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC endpoint.
    endpointStatus :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of subnets where Amazon Redshift Serverless choose
    -- to deploy the VPC endpoint.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the VPC endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The connection endpoint for connecting to Amazon Redshift Serverless.
    vpcEndpoint :: Prelude.Maybe VpcEndpoint,
    -- | The security groups associated with the endpoint.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | The time that the endpoint was created.
    endpointCreateTime :: Prelude.Maybe Core.POSIX
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
-- 'port', 'endpointAccess_port' - The port number on which Amazon Redshift Serverless accepts incoming
-- connections.
--
-- 'endpointName', 'endpointAccess_endpointName' - The name of the VPC endpoint.
--
-- 'workgroupName', 'endpointAccess_workgroupName' - The name of the workgroup associated with the endpoint.
--
-- 'address', 'endpointAccess_address' - The DNS address of the endpoint.
--
-- 'endpointStatus', 'endpointAccess_endpointStatus' - The status of the VPC endpoint.
--
-- 'subnetIds', 'endpointAccess_subnetIds' - The unique identifier of subnets where Amazon Redshift Serverless choose
-- to deploy the VPC endpoint.
--
-- 'endpointArn', 'endpointAccess_endpointArn' - The Amazon Resource Name (ARN) of the VPC endpoint.
--
-- 'vpcEndpoint', 'endpointAccess_vpcEndpoint' - The connection endpoint for connecting to Amazon Redshift Serverless.
--
-- 'vpcSecurityGroups', 'endpointAccess_vpcSecurityGroups' - The security groups associated with the endpoint.
--
-- 'endpointCreateTime', 'endpointAccess_endpointCreateTime' - The time that the endpoint was created.
newEndpointAccess ::
  EndpointAccess
newEndpointAccess =
  EndpointAccess'
    { port = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      address = Prelude.Nothing,
      endpointStatus = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      vpcEndpoint = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      endpointCreateTime = Prelude.Nothing
    }

-- | The port number on which Amazon Redshift Serverless accepts incoming
-- connections.
endpointAccess_port :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Int)
endpointAccess_port = Lens.lens (\EndpointAccess' {port} -> port) (\s@EndpointAccess' {} a -> s {port = a} :: EndpointAccess)

-- | The name of the VPC endpoint.
endpointAccess_endpointName :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointName = Lens.lens (\EndpointAccess' {endpointName} -> endpointName) (\s@EndpointAccess' {} a -> s {endpointName = a} :: EndpointAccess)

-- | The name of the workgroup associated with the endpoint.
endpointAccess_workgroupName :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_workgroupName = Lens.lens (\EndpointAccess' {workgroupName} -> workgroupName) (\s@EndpointAccess' {} a -> s {workgroupName = a} :: EndpointAccess)

-- | The DNS address of the endpoint.
endpointAccess_address :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_address = Lens.lens (\EndpointAccess' {address} -> address) (\s@EndpointAccess' {} a -> s {address = a} :: EndpointAccess)

-- | The status of the VPC endpoint.
endpointAccess_endpointStatus :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointStatus = Lens.lens (\EndpointAccess' {endpointStatus} -> endpointStatus) (\s@EndpointAccess' {} a -> s {endpointStatus = a} :: EndpointAccess)

-- | The unique identifier of subnets where Amazon Redshift Serverless choose
-- to deploy the VPC endpoint.
endpointAccess_subnetIds :: Lens.Lens' EndpointAccess (Prelude.Maybe [Prelude.Text])
endpointAccess_subnetIds = Lens.lens (\EndpointAccess' {subnetIds} -> subnetIds) (\s@EndpointAccess' {} a -> s {subnetIds = a} :: EndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the VPC endpoint.
endpointAccess_endpointArn :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.Text)
endpointAccess_endpointArn = Lens.lens (\EndpointAccess' {endpointArn} -> endpointArn) (\s@EndpointAccess' {} a -> s {endpointArn = a} :: EndpointAccess)

-- | The connection endpoint for connecting to Amazon Redshift Serverless.
endpointAccess_vpcEndpoint :: Lens.Lens' EndpointAccess (Prelude.Maybe VpcEndpoint)
endpointAccess_vpcEndpoint = Lens.lens (\EndpointAccess' {vpcEndpoint} -> vpcEndpoint) (\s@EndpointAccess' {} a -> s {vpcEndpoint = a} :: EndpointAccess)

-- | The security groups associated with the endpoint.
endpointAccess_vpcSecurityGroups :: Lens.Lens' EndpointAccess (Prelude.Maybe [VpcSecurityGroupMembership])
endpointAccess_vpcSecurityGroups = Lens.lens (\EndpointAccess' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@EndpointAccess' {} a -> s {vpcSecurityGroups = a} :: EndpointAccess) Prelude.. Lens.mapping Lens.coerced

-- | The time that the endpoint was created.
endpointAccess_endpointCreateTime :: Lens.Lens' EndpointAccess (Prelude.Maybe Prelude.UTCTime)
endpointAccess_endpointCreateTime = Lens.lens (\EndpointAccess' {endpointCreateTime} -> endpointCreateTime) (\s@EndpointAccess' {} a -> s {endpointCreateTime = a} :: EndpointAccess) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON EndpointAccess where
  parseJSON =
    Core.withObject
      "EndpointAccess"
      ( \x ->
          EndpointAccess'
            Prelude.<$> (x Core..:? "port")
            Prelude.<*> (x Core..:? "endpointName")
            Prelude.<*> (x Core..:? "workgroupName")
            Prelude.<*> (x Core..:? "address")
            Prelude.<*> (x Core..:? "endpointStatus")
            Prelude.<*> (x Core..:? "subnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "endpointArn")
            Prelude.<*> (x Core..:? "vpcEndpoint")
            Prelude.<*> ( x Core..:? "vpcSecurityGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "endpointCreateTime")
      )

instance Prelude.Hashable EndpointAccess where
  hashWithSalt _salt EndpointAccess' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` vpcEndpoint
      `Prelude.hashWithSalt` vpcSecurityGroups
      `Prelude.hashWithSalt` endpointCreateTime

instance Prelude.NFData EndpointAccess where
  rnf EndpointAccess' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf vpcEndpoint
      `Prelude.seq` Prelude.rnf vpcSecurityGroups
      `Prelude.seq` Prelude.rnf endpointCreateTime
