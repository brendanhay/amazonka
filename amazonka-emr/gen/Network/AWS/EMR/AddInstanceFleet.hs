{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.AddInstanceFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an instance fleet to a running cluster.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x.
module Network.AWS.EMR.AddInstanceFleet
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text,
    -- | Specifies the configuration of the instance fleet.
    instanceFleet :: InstanceFleetConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AddInstanceFleet where
  type Rs AddInstanceFleet = AddInstanceFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddInstanceFleetResponse'
            Prelude.<$> (x Prelude..?> "ClusterArn")
            Prelude.<*> (x Prelude..?> "ClusterId")
            Prelude.<*> (x Prelude..?> "InstanceFleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddInstanceFleet

instance Prelude.NFData AddInstanceFleet

instance Prelude.ToHeaders AddInstanceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.AddInstanceFleet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddInstanceFleet where
  toJSON AddInstanceFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Prelude..= clusterId),
            Prelude.Just
              ("InstanceFleet" Prelude..= instanceFleet)
          ]
      )

instance Prelude.ToPath AddInstanceFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddInstanceFleet where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AddInstanceFleetResponse
