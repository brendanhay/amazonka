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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'newSetVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
  { -- | The unique identifier of the job flow (cluster).
    jobFlowIds :: [Prelude.Text],
    -- | A value of @true@ indicates that all IAM users in the AWS account can
    -- perform cluster actions if they have the proper IAM policy permissions.
    -- This is the default. A value of @false@ indicates that only the IAM user
    -- who created the cluster can perform actions.
    visibleToAllUsers :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Bool ->
  SetVisibleToAllUsers
newSetVisibleToAllUsers pVisibleToAllUsers_ =
  SetVisibleToAllUsers'
    { jobFlowIds = Prelude.mempty,
      visibleToAllUsers = pVisibleToAllUsers_
    }

-- | The unique identifier of the job flow (cluster).
setVisibleToAllUsers_jobFlowIds :: Lens.Lens' SetVisibleToAllUsers [Prelude.Text]
setVisibleToAllUsers_jobFlowIds = Lens.lens (\SetVisibleToAllUsers' {jobFlowIds} -> jobFlowIds) (\s@SetVisibleToAllUsers' {} a -> s {jobFlowIds = a} :: SetVisibleToAllUsers) Prelude.. Prelude._Coerce

-- | A value of @true@ indicates that all IAM users in the AWS account can
-- perform cluster actions if they have the proper IAM policy permissions.
-- This is the default. A value of @false@ indicates that only the IAM user
-- who created the cluster can perform actions.
setVisibleToAllUsers_visibleToAllUsers :: Lens.Lens' SetVisibleToAllUsers Prelude.Bool
setVisibleToAllUsers_visibleToAllUsers = Lens.lens (\SetVisibleToAllUsers' {visibleToAllUsers} -> visibleToAllUsers) (\s@SetVisibleToAllUsers' {} a -> s {visibleToAllUsers = a} :: SetVisibleToAllUsers)

instance Prelude.AWSRequest SetVisibleToAllUsers where
  type
    Rs SetVisibleToAllUsers =
      SetVisibleToAllUsersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetVisibleToAllUsersResponse'

instance Prelude.Hashable SetVisibleToAllUsers

instance Prelude.NFData SetVisibleToAllUsers

instance Prelude.ToHeaders SetVisibleToAllUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.SetVisibleToAllUsers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetVisibleToAllUsers where
  toJSON SetVisibleToAllUsers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobFlowIds" Prelude..= jobFlowIds),
            Prelude.Just
              ("VisibleToAllUsers" Prelude..= visibleToAllUsers)
          ]
      )

instance Prelude.ToPath SetVisibleToAllUsers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetVisibleToAllUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetVisibleToAllUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetVisibleToAllUsersResponse ::
  SetVisibleToAllUsersResponse
newSetVisibleToAllUsersResponse =
  SetVisibleToAllUsersResponse'

instance Prelude.NFData SetVisibleToAllUsersResponse
