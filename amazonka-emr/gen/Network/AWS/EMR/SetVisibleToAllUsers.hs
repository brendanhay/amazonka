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
-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Cluster$VisibleToAllUsers value, which determines whether the
-- cluster is visible to all IAM users of the AWS account associated with
-- the cluster. Only the IAM user who created the cluster or the AWS
-- account root user can call this action. The default value, @true@,
-- indicates that all IAM users in the AWS account can perform cluster
-- actions if they have the proper IAM policy permissions. If set to
-- @false@, only the IAM user that created the cluster can perform actions.
-- This action works on running clusters. You can override the default
-- @true@ setting when you create a cluster by using the
-- @VisibleToAllUsers@ parameter with @RunJobFlow@.
module Network.AWS.EMR.SetVisibleToAllUsers
  ( -- * Creating a Request
    SetVisibleToAllUsers (..),
    newSetVisibleToAllUsers,

    -- * Request Lenses
    setVisibleToAllUsers_jobFlowIds,
    setVisibleToAllUsers_visibleToAllUsers,

    -- * Destructuring the Response
    SetVisibleToAllUsersResponse (..),
    newSetVisibleToAllUsersResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'newSetVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
  { -- | The unique identifier of the job flow (cluster).
    jobFlowIds :: [Core.Text],
    -- | A value of @true@ indicates that all IAM users in the AWS account can
    -- perform cluster actions if they have the proper IAM policy permissions.
    -- This is the default. A value of @false@ indicates that only the IAM user
    -- who created the cluster can perform actions.
    visibleToAllUsers :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetVisibleToAllUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobFlowIds', 'setVisibleToAllUsers_jobFlowIds' - The unique identifier of the job flow (cluster).
--
-- 'visibleToAllUsers', 'setVisibleToAllUsers_visibleToAllUsers' - A value of @true@ indicates that all IAM users in the AWS account can
-- perform cluster actions if they have the proper IAM policy permissions.
-- This is the default. A value of @false@ indicates that only the IAM user
-- who created the cluster can perform actions.
newSetVisibleToAllUsers ::
  -- | 'visibleToAllUsers'
  Core.Bool ->
  SetVisibleToAllUsers
newSetVisibleToAllUsers pVisibleToAllUsers_ =
  SetVisibleToAllUsers'
    { jobFlowIds = Core.mempty,
      visibleToAllUsers = pVisibleToAllUsers_
    }

-- | The unique identifier of the job flow (cluster).
setVisibleToAllUsers_jobFlowIds :: Lens.Lens' SetVisibleToAllUsers [Core.Text]
setVisibleToAllUsers_jobFlowIds = Lens.lens (\SetVisibleToAllUsers' {jobFlowIds} -> jobFlowIds) (\s@SetVisibleToAllUsers' {} a -> s {jobFlowIds = a} :: SetVisibleToAllUsers) Core.. Lens._Coerce

-- | A value of @true@ indicates that all IAM users in the AWS account can
-- perform cluster actions if they have the proper IAM policy permissions.
-- This is the default. A value of @false@ indicates that only the IAM user
-- who created the cluster can perform actions.
setVisibleToAllUsers_visibleToAllUsers :: Lens.Lens' SetVisibleToAllUsers Core.Bool
setVisibleToAllUsers_visibleToAllUsers = Lens.lens (\SetVisibleToAllUsers' {visibleToAllUsers} -> visibleToAllUsers) (\s@SetVisibleToAllUsers' {} a -> s {visibleToAllUsers = a} :: SetVisibleToAllUsers)

instance Core.AWSRequest SetVisibleToAllUsers where
  type
    AWSResponse SetVisibleToAllUsers =
      SetVisibleToAllUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetVisibleToAllUsersResponse'

instance Core.Hashable SetVisibleToAllUsers

instance Core.NFData SetVisibleToAllUsers

instance Core.ToHeaders SetVisibleToAllUsers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.SetVisibleToAllUsers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetVisibleToAllUsers where
  toJSON SetVisibleToAllUsers' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobFlowIds" Core..= jobFlowIds),
            Core.Just
              ("VisibleToAllUsers" Core..= visibleToAllUsers)
          ]
      )

instance Core.ToPath SetVisibleToAllUsers where
  toPath = Core.const "/"

instance Core.ToQuery SetVisibleToAllUsers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetVisibleToAllUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetVisibleToAllUsersResponse ::
  SetVisibleToAllUsersResponse
newSetVisibleToAllUsersResponse =
  SetVisibleToAllUsersResponse'

instance Core.NFData SetVisibleToAllUsersResponse
