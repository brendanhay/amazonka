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
-- Module      : Network.AWS.EMR.ModifyInstanceFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target On-Demand and target Spot capacities for the
-- instance fleet with the specified InstanceFleetID within the cluster
-- specified using ClusterID. The call either succeeds or fails atomically.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
module Network.AWS.EMR.ModifyInstanceFleet
  ( -- * Creating a Request
    ModifyInstanceFleet (..),
    newModifyInstanceFleet,

    -- * Request Lenses
    modifyInstanceFleet_clusterId,
    modifyInstanceFleet_instanceFleet,

    -- * Destructuring the Response
    ModifyInstanceFleetResponse (..),
    newModifyInstanceFleetResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyInstanceFleet' smart constructor.
data ModifyInstanceFleet = ModifyInstanceFleet'
  { -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text,
    -- | The unique identifier of the instance fleet.
    instanceFleet :: InstanceFleetModifyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'modifyInstanceFleet_clusterId' - The unique identifier of the cluster.
--
-- 'instanceFleet', 'modifyInstanceFleet_instanceFleet' - The unique identifier of the instance fleet.
newModifyInstanceFleet ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'instanceFleet'
  InstanceFleetModifyConfig ->
  ModifyInstanceFleet
newModifyInstanceFleet pClusterId_ pInstanceFleet_ =
  ModifyInstanceFleet'
    { clusterId = pClusterId_,
      instanceFleet = pInstanceFleet_
    }

-- | The unique identifier of the cluster.
modifyInstanceFleet_clusterId :: Lens.Lens' ModifyInstanceFleet Prelude.Text
modifyInstanceFleet_clusterId = Lens.lens (\ModifyInstanceFleet' {clusterId} -> clusterId) (\s@ModifyInstanceFleet' {} a -> s {clusterId = a} :: ModifyInstanceFleet)

-- | The unique identifier of the instance fleet.
modifyInstanceFleet_instanceFleet :: Lens.Lens' ModifyInstanceFleet InstanceFleetModifyConfig
modifyInstanceFleet_instanceFleet = Lens.lens (\ModifyInstanceFleet' {instanceFleet} -> instanceFleet) (\s@ModifyInstanceFleet' {} a -> s {instanceFleet = a} :: ModifyInstanceFleet)

instance Prelude.AWSRequest ModifyInstanceFleet where
  type
    Rs ModifyInstanceFleet =
      ModifyInstanceFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ModifyInstanceFleetResponse'

instance Prelude.Hashable ModifyInstanceFleet

instance Prelude.NFData ModifyInstanceFleet

instance Prelude.ToHeaders ModifyInstanceFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.ModifyInstanceFleet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyInstanceFleet where
  toJSON ModifyInstanceFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Prelude..= clusterId),
            Prelude.Just
              ("InstanceFleet" Prelude..= instanceFleet)
          ]
      )

instance Prelude.ToPath ModifyInstanceFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyInstanceFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyInstanceFleetResponse' smart constructor.
data ModifyInstanceFleetResponse = ModifyInstanceFleetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyInstanceFleetResponse ::
  ModifyInstanceFleetResponse
newModifyInstanceFleetResponse =
  ModifyInstanceFleetResponse'

instance Prelude.NFData ModifyInstanceFleetResponse
