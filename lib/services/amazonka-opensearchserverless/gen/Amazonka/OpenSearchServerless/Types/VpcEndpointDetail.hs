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
-- Module      : Amazonka.OpenSearchServerless.Types.VpcEndpointDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.VpcEndpointDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about an OpenSearch Serverless-managed interface endpoint.
--
-- /See:/ 'newVpcEndpointDetail' smart constructor.
data VpcEndpointDetail = VpcEndpointDetail'
  { -- | The date the endpoint was created.
    createdDate :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier of the endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifiers of the security groups that define the ports,
    -- protocols, and sources for inbound traffic that you are authorizing into
    -- your endpoint.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The current status of the endpoint.
    status :: Prelude.Maybe VpcEndpointStatus,
    -- | The ID of the subnets from which you access OpenSearch Serverless.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the VPC from which you access OpenSearch Serverless
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpointDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'vpcEndpointDetail_createdDate' - The date the endpoint was created.
--
-- 'id', 'vpcEndpointDetail_id' - The unique identifier of the endpoint.
--
-- 'name', 'vpcEndpointDetail_name' - The name of the endpoint.
--
-- 'securityGroupIds', 'vpcEndpointDetail_securityGroupIds' - The unique identifiers of the security groups that define the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
--
-- 'status', 'vpcEndpointDetail_status' - The current status of the endpoint.
--
-- 'subnetIds', 'vpcEndpointDetail_subnetIds' - The ID of the subnets from which you access OpenSearch Serverless.
--
-- 'vpcId', 'vpcEndpointDetail_vpcId' - The ID of the VPC from which you access OpenSearch Serverless
newVpcEndpointDetail ::
  VpcEndpointDetail
newVpcEndpointDetail =
  VpcEndpointDetail'
    { createdDate = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      status = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The date the endpoint was created.
vpcEndpointDetail_createdDate :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe Prelude.Integer)
vpcEndpointDetail_createdDate = Lens.lens (\VpcEndpointDetail' {createdDate} -> createdDate) (\s@VpcEndpointDetail' {} a -> s {createdDate = a} :: VpcEndpointDetail)

-- | The unique identifier of the endpoint.
vpcEndpointDetail_id :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe Prelude.Text)
vpcEndpointDetail_id = Lens.lens (\VpcEndpointDetail' {id} -> id) (\s@VpcEndpointDetail' {} a -> s {id = a} :: VpcEndpointDetail)

-- | The name of the endpoint.
vpcEndpointDetail_name :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe Prelude.Text)
vpcEndpointDetail_name = Lens.lens (\VpcEndpointDetail' {name} -> name) (\s@VpcEndpointDetail' {} a -> s {name = a} :: VpcEndpointDetail)

-- | The unique identifiers of the security groups that define the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
vpcEndpointDetail_securityGroupIds :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vpcEndpointDetail_securityGroupIds = Lens.lens (\VpcEndpointDetail' {securityGroupIds} -> securityGroupIds) (\s@VpcEndpointDetail' {} a -> s {securityGroupIds = a} :: VpcEndpointDetail) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the endpoint.
vpcEndpointDetail_status :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe VpcEndpointStatus)
vpcEndpointDetail_status = Lens.lens (\VpcEndpointDetail' {status} -> status) (\s@VpcEndpointDetail' {} a -> s {status = a} :: VpcEndpointDetail)

-- | The ID of the subnets from which you access OpenSearch Serverless.
vpcEndpointDetail_subnetIds :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vpcEndpointDetail_subnetIds = Lens.lens (\VpcEndpointDetail' {subnetIds} -> subnetIds) (\s@VpcEndpointDetail' {} a -> s {subnetIds = a} :: VpcEndpointDetail) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC from which you access OpenSearch Serverless
vpcEndpointDetail_vpcId :: Lens.Lens' VpcEndpointDetail (Prelude.Maybe Prelude.Text)
vpcEndpointDetail_vpcId = Lens.lens (\VpcEndpointDetail' {vpcId} -> vpcId) (\s@VpcEndpointDetail' {} a -> s {vpcId = a} :: VpcEndpointDetail)

instance Data.FromJSON VpcEndpointDetail where
  parseJSON =
    Data.withObject
      "VpcEndpointDetail"
      ( \x ->
          VpcEndpointDetail'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "securityGroupIds")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "subnetIds")
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable VpcEndpointDetail where
  hashWithSalt _salt VpcEndpointDetail' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcEndpointDetail where
  rnf VpcEndpointDetail' {..} =
    Prelude.rnf createdDate `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf securityGroupIds `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf subnetIds `Prelude.seq`
                Prelude.rnf vpcId
