{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.AddInstanceFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an instance fleet to a running cluster.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x.
module Amazonka.EMR.AddInstanceFleet
  ( -- * Creating a Request
    AddInstanceFleet (..),
    newAddInstanceFleet,

    -- * Request Lenses
    addInstanceFleet_clusterId,
    addInstanceFleet_instanceFleet,

    -- * Destructuring the Response
    AddInstanceFleetResponse (..),
    newAddInstanceFleetResponse,

    -- * Response Lenses
    addInstanceFleetResponse_clusterArn,
    addInstanceFleetResponse_clusterId,
    addInstanceFleetResponse_instanceFleetId,
    addInstanceFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text,
    -- | Specifies the configuration of the instance fleet.
    instanceFleet :: InstanceFleetConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddInstanceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'addInstanceFleet_clusterId' - The unique identifier of the cluster.
--
-- 'instanceFleet', 'addInstanceFleet_instanceFleet' - Specifies the configuration of the instance fleet.
newAddInstanceFleet ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'instanceFleet'
  InstanceFleetConfig ->
  AddInstanceFleet
newAddInstanceFleet pClusterId_ pInstanceFleet_ =
  AddInstanceFleet'
    { clusterId = pClusterId_,
      instanceFleet = pInstanceFleet_
    }

-- | The unique identifier of the cluster.
addInstanceFleet_clusterId :: Lens.Lens' AddInstanceFleet Prelude.Text
addInstanceFleet_clusterId = Lens.lens (\AddInstanceFleet' {clusterId} -> clusterId) (\s@AddInstanceFleet' {} a -> s {clusterId = a} :: AddInstanceFleet)

-- | Specifies the configuration of the instance fleet.
addInstanceFleet_instanceFleet :: Lens.Lens' AddInstanceFleet InstanceFleetConfig
addInstanceFleet_instanceFleet = Lens.lens (\AddInstanceFleet' {instanceFleet} -> instanceFleet) (\s@AddInstanceFleet' {} a -> s {instanceFleet = a} :: AddInstanceFleet)

instance Core.AWSRequest AddInstanceFleet where
  type
    AWSResponse AddInstanceFleet =
      AddInstanceFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddInstanceFleetResponse'
            Prelude.<$> (x Data..?> "ClusterArn")
            Prelude.<*> (x Data..?> "ClusterId")
            Prelude.<*> (x Data..?> "InstanceFleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddInstanceFleet where
  hashWithSalt _salt AddInstanceFleet' {..} =
    _salt `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` instanceFleet

instance Prelude.NFData AddInstanceFleet where
  rnf AddInstanceFleet' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf instanceFleet

instance Data.ToHeaders AddInstanceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.AddInstanceFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddInstanceFleet where
  toJSON AddInstanceFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Data..= clusterId),
            Prelude.Just
              ("InstanceFleet" Data..= instanceFleet)
          ]
      )

instance Data.ToPath AddInstanceFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery AddInstanceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddInstanceFleetResponse' smart constructor.
data AddInstanceFleetResponse = AddInstanceFleetResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddInstanceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'addInstanceFleetResponse_clusterArn' - The Amazon Resource Name of the cluster.
--
-- 'clusterId', 'addInstanceFleetResponse_clusterId' - The unique identifier of the cluster.
--
-- 'instanceFleetId', 'addInstanceFleetResponse_instanceFleetId' - The unique identifier of the instance fleet.
--
-- 'httpStatus', 'addInstanceFleetResponse_httpStatus' - The response's http status code.
newAddInstanceFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddInstanceFleetResponse
newAddInstanceFleetResponse pHttpStatus_ =
  AddInstanceFleetResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterId = Prelude.Nothing,
      instanceFleetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name of the cluster.
addInstanceFleetResponse_clusterArn :: Lens.Lens' AddInstanceFleetResponse (Prelude.Maybe Prelude.Text)
addInstanceFleetResponse_clusterArn = Lens.lens (\AddInstanceFleetResponse' {clusterArn} -> clusterArn) (\s@AddInstanceFleetResponse' {} a -> s {clusterArn = a} :: AddInstanceFleetResponse)

-- | The unique identifier of the cluster.
addInstanceFleetResponse_clusterId :: Lens.Lens' AddInstanceFleetResponse (Prelude.Maybe Prelude.Text)
addInstanceFleetResponse_clusterId = Lens.lens (\AddInstanceFleetResponse' {clusterId} -> clusterId) (\s@AddInstanceFleetResponse' {} a -> s {clusterId = a} :: AddInstanceFleetResponse)

-- | The unique identifier of the instance fleet.
addInstanceFleetResponse_instanceFleetId :: Lens.Lens' AddInstanceFleetResponse (Prelude.Maybe Prelude.Text)
addInstanceFleetResponse_instanceFleetId = Lens.lens (\AddInstanceFleetResponse' {instanceFleetId} -> instanceFleetId) (\s@AddInstanceFleetResponse' {} a -> s {instanceFleetId = a} :: AddInstanceFleetResponse)

-- | The response's http status code.
addInstanceFleetResponse_httpStatus :: Lens.Lens' AddInstanceFleetResponse Prelude.Int
addInstanceFleetResponse_httpStatus = Lens.lens (\AddInstanceFleetResponse' {httpStatus} -> httpStatus) (\s@AddInstanceFleetResponse' {} a -> s {httpStatus = a} :: AddInstanceFleetResponse)

instance Prelude.NFData AddInstanceFleetResponse where
  rnf AddInstanceFleetResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf instanceFleetId
      `Prelude.seq` Prelude.rnf httpStatus
