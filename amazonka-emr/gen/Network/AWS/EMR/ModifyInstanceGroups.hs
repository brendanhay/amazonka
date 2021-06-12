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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Change the size of some instance groups.
--
-- /See:/ 'newModifyInstanceGroups' smart constructor.
data ModifyInstanceGroups = ModifyInstanceGroups'
  { -- | The ID of the cluster to which the instance group belongs.
    clusterId :: Core.Maybe Core.Text,
    -- | Instance groups to change.
    instanceGroups :: Core.Maybe [InstanceGroupModifyConfig]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { clusterId = Core.Nothing,
      instanceGroups = Core.Nothing
    }

-- | The ID of the cluster to which the instance group belongs.
modifyInstanceGroups_clusterId :: Lens.Lens' ModifyInstanceGroups (Core.Maybe Core.Text)
modifyInstanceGroups_clusterId = Lens.lens (\ModifyInstanceGroups' {clusterId} -> clusterId) (\s@ModifyInstanceGroups' {} a -> s {clusterId = a} :: ModifyInstanceGroups)

-- | Instance groups to change.
modifyInstanceGroups_instanceGroups :: Lens.Lens' ModifyInstanceGroups (Core.Maybe [InstanceGroupModifyConfig])
modifyInstanceGroups_instanceGroups = Lens.lens (\ModifyInstanceGroups' {instanceGroups} -> instanceGroups) (\s@ModifyInstanceGroups' {} a -> s {instanceGroups = a} :: ModifyInstanceGroups) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest ModifyInstanceGroups where
  type
    AWSResponse ModifyInstanceGroups =
      ModifyInstanceGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ModifyInstanceGroupsResponse'

instance Core.Hashable ModifyInstanceGroups

instance Core.NFData ModifyInstanceGroups

instance Core.ToHeaders ModifyInstanceGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.ModifyInstanceGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyInstanceGroups where
  toJSON ModifyInstanceGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClusterId" Core..=) Core.<$> clusterId,
            ("InstanceGroups" Core..=) Core.<$> instanceGroups
          ]
      )

instance Core.ToPath ModifyInstanceGroups where
  toPath = Core.const "/"

instance Core.ToQuery ModifyInstanceGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyInstanceGroupsResponse' smart constructor.
data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyInstanceGroupsResponse ::
  ModifyInstanceGroupsResponse
newModifyInstanceGroupsResponse =
  ModifyInstanceGroupsResponse'

instance Core.NFData ModifyInstanceGroupsResponse
