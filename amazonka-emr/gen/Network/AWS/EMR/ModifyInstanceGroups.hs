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
-- Module      : Network.AWS.EMR.ModifyInstanceGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ModifyInstanceGroups modifies the number of nodes and configuration
-- settings of an instance group. The input parameters include the new
-- target instance count for the group and the instance group ID. The call
-- will either succeed or fail atomically.
module Network.AWS.EMR.ModifyInstanceGroups
  ( -- * Creating a Request
    ModifyInstanceGroups (..),
    newModifyInstanceGroups,

    -- * Request Lenses
    modifyInstanceGroups_clusterId,
    modifyInstanceGroups_instanceGroups,

    -- * Destructuring the Response
    ModifyInstanceGroupsResponse (..),
    newModifyInstanceGroupsResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Change the size of some instance groups.
--
-- /See:/ 'newModifyInstanceGroups' smart constructor.
data ModifyInstanceGroups = ModifyInstanceGroups'
  { -- | The ID of the cluster to which the instance group belongs.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | Instance groups to change.
    instanceGroups :: Prelude.Maybe [InstanceGroupModifyConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'modifyInstanceGroups_clusterId' - The ID of the cluster to which the instance group belongs.
--
-- 'instanceGroups', 'modifyInstanceGroups_instanceGroups' - Instance groups to change.
newModifyInstanceGroups ::
  ModifyInstanceGroups
newModifyInstanceGroups =
  ModifyInstanceGroups'
    { clusterId = Prelude.Nothing,
      instanceGroups = Prelude.Nothing
    }

-- | The ID of the cluster to which the instance group belongs.
modifyInstanceGroups_clusterId :: Lens.Lens' ModifyInstanceGroups (Prelude.Maybe Prelude.Text)
modifyInstanceGroups_clusterId = Lens.lens (\ModifyInstanceGroups' {clusterId} -> clusterId) (\s@ModifyInstanceGroups' {} a -> s {clusterId = a} :: ModifyInstanceGroups)

-- | Instance groups to change.
modifyInstanceGroups_instanceGroups :: Lens.Lens' ModifyInstanceGroups (Prelude.Maybe [InstanceGroupModifyConfig])
modifyInstanceGroups_instanceGroups = Lens.lens (\ModifyInstanceGroups' {instanceGroups} -> instanceGroups) (\s@ModifyInstanceGroups' {} a -> s {instanceGroups = a} :: ModifyInstanceGroups) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest ModifyInstanceGroups where
  type
    Rs ModifyInstanceGroups =
      ModifyInstanceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ModifyInstanceGroupsResponse'

instance Prelude.Hashable ModifyInstanceGroups

instance Prelude.NFData ModifyInstanceGroups

instance Prelude.ToHeaders ModifyInstanceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.ModifyInstanceGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ModifyInstanceGroups where
  toJSON ModifyInstanceGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClusterId" Prelude..=) Prelude.<$> clusterId,
            ("InstanceGroups" Prelude..=)
              Prelude.<$> instanceGroups
          ]
      )

instance Prelude.ToPath ModifyInstanceGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyInstanceGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyInstanceGroupsResponse' smart constructor.
data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyInstanceGroupsResponse ::
  ModifyInstanceGroupsResponse
newModifyInstanceGroupsResponse =
  ModifyInstanceGroupsResponse'

instance Prelude.NFData ModifyInstanceGroupsResponse
