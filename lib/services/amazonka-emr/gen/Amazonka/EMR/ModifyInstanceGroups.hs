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
-- Module      : Amazonka.EMR.ModifyInstanceGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ModifyInstanceGroups modifies the number of nodes and configuration
-- settings of an instance group. The input parameters include the new
-- target instance count for the group and the instance group ID. The call
-- will either succeed or fail atomically.
module Amazonka.EMR.ModifyInstanceGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Change the size of some instance groups.
--
-- /See:/ 'newModifyInstanceGroups' smart constructor.
data ModifyInstanceGroups = ModifyInstanceGroups'
  { -- | The ID of the cluster to which the instance group belongs.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | Instance groups to change.
    instanceGroups :: Prelude.Maybe [InstanceGroupModifyConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
modifyInstanceGroups_instanceGroups = Lens.lens (\ModifyInstanceGroups' {instanceGroups} -> instanceGroups) (\s@ModifyInstanceGroups' {} a -> s {instanceGroups = a} :: ModifyInstanceGroups) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ModifyInstanceGroups where
  type
    AWSResponse ModifyInstanceGroups =
      ModifyInstanceGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ModifyInstanceGroupsResponse'

instance Prelude.Hashable ModifyInstanceGroups where
  hashWithSalt _salt ModifyInstanceGroups' {..} =
    _salt `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` instanceGroups

instance Prelude.NFData ModifyInstanceGroups where
  rnf ModifyInstanceGroups' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf instanceGroups

instance Data.ToHeaders ModifyInstanceGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.ModifyInstanceGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyInstanceGroups where
  toJSON ModifyInstanceGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterId" Data..=) Prelude.<$> clusterId,
            ("InstanceGroups" Data..=)
              Prelude.<$> instanceGroups
          ]
      )

instance Data.ToPath ModifyInstanceGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyInstanceGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyInstanceGroupsResponse' smart constructor.
data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyInstanceGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifyInstanceGroupsResponse ::
  ModifyInstanceGroupsResponse
newModifyInstanceGroupsResponse =
  ModifyInstanceGroupsResponse'

instance Prelude.NFData ModifyInstanceGroupsResponse where
  rnf _ = ()
