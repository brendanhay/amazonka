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
-- Module      : Amazonka.OpenSearchServerless.Types.UpdateVpcEndpointDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.UpdateVpcEndpointDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import qualified Amazonka.Prelude as Prelude

-- | Update details for an OpenSearch Serverless-managed interface endpoint.
--
-- /See:/ 'newUpdateVpcEndpointDetail' smart constructor.
data UpdateVpcEndpointDetail = UpdateVpcEndpointDetail'
  { -- | The unique identifier of the endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the endpoint was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Integer,
    -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifiers of the security groups that define the ports,
    -- protocols, and sources for inbound traffic that you are authorizing into
    -- your endpoint.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The current status of the endpoint update process.
    status :: Prelude.Maybe VpcEndpointStatus,
    -- | The ID of the subnets from which you access OpenSearch Serverless.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcEndpointDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateVpcEndpointDetail_id' - The unique identifier of the endpoint.
--
-- 'lastModifiedDate', 'updateVpcEndpointDetail_lastModifiedDate' - The timestamp of when the endpoint was last modified.
--
-- 'name', 'updateVpcEndpointDetail_name' - The name of the endpoint.
--
-- 'securityGroupIds', 'updateVpcEndpointDetail_securityGroupIds' - The unique identifiers of the security groups that define the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
--
-- 'status', 'updateVpcEndpointDetail_status' - The current status of the endpoint update process.
--
-- 'subnetIds', 'updateVpcEndpointDetail_subnetIds' - The ID of the subnets from which you access OpenSearch Serverless.
newUpdateVpcEndpointDetail ::
  UpdateVpcEndpointDetail
newUpdateVpcEndpointDetail =
  UpdateVpcEndpointDetail'
    { id = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      status = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The unique identifier of the endpoint.
updateVpcEndpointDetail_id :: Lens.Lens' UpdateVpcEndpointDetail (Prelude.Maybe Prelude.Text)
updateVpcEndpointDetail_id = Lens.lens (\UpdateVpcEndpointDetail' {id} -> id) (\s@UpdateVpcEndpointDetail' {} a -> s {id = a} :: UpdateVpcEndpointDetail)

-- | The timestamp of when the endpoint was last modified.
updateVpcEndpointDetail_lastModifiedDate :: Lens.Lens' UpdateVpcEndpointDetail (Prelude.Maybe Prelude.Integer)
updateVpcEndpointDetail_lastModifiedDate = Lens.lens (\UpdateVpcEndpointDetail' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateVpcEndpointDetail' {} a -> s {lastModifiedDate = a} :: UpdateVpcEndpointDetail)

-- | The name of the endpoint.
updateVpcEndpointDetail_name :: Lens.Lens' UpdateVpcEndpointDetail (Prelude.Maybe Prelude.Text)
updateVpcEndpointDetail_name = Lens.lens (\UpdateVpcEndpointDetail' {name} -> name) (\s@UpdateVpcEndpointDetail' {} a -> s {name = a} :: UpdateVpcEndpointDetail)

-- | The unique identifiers of the security groups that define the ports,
-- protocols, and sources for inbound traffic that you are authorizing into
-- your endpoint.
updateVpcEndpointDetail_securityGroupIds :: Lens.Lens' UpdateVpcEndpointDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVpcEndpointDetail_securityGroupIds = Lens.lens (\UpdateVpcEndpointDetail' {securityGroupIds} -> securityGroupIds) (\s@UpdateVpcEndpointDetail' {} a -> s {securityGroupIds = a} :: UpdateVpcEndpointDetail) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the endpoint update process.
updateVpcEndpointDetail_status :: Lens.Lens' UpdateVpcEndpointDetail (Prelude.Maybe VpcEndpointStatus)
updateVpcEndpointDetail_status = Lens.lens (\UpdateVpcEndpointDetail' {status} -> status) (\s@UpdateVpcEndpointDetail' {} a -> s {status = a} :: UpdateVpcEndpointDetail)

-- | The ID of the subnets from which you access OpenSearch Serverless.
updateVpcEndpointDetail_subnetIds :: Lens.Lens' UpdateVpcEndpointDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateVpcEndpointDetail_subnetIds = Lens.lens (\UpdateVpcEndpointDetail' {subnetIds} -> subnetIds) (\s@UpdateVpcEndpointDetail' {} a -> s {subnetIds = a} :: UpdateVpcEndpointDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UpdateVpcEndpointDetail where
  parseJSON =
    Data.withObject
      "UpdateVpcEndpointDetail"
      ( \x ->
          UpdateVpcEndpointDetail'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "securityGroupIds")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "subnetIds")
      )

instance Prelude.Hashable UpdateVpcEndpointDetail where
  hashWithSalt _salt UpdateVpcEndpointDetail' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData UpdateVpcEndpointDetail where
  rnf UpdateVpcEndpointDetail' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnetIds
