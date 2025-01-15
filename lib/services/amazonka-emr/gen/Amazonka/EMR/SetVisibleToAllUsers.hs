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
-- Module      : Amazonka.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The SetVisibleToAllUsers parameter is no longer supported. Your cluster
-- may be visible to all users in your account. To restrict cluster access
-- using an IAM policy, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-access-iam.html Identity and Access Management for EMR>.
--
-- Sets the Cluster$VisibleToAllUsers value for an EMR cluster. When
-- @true@, IAM principals in the Amazon Web Services account can perform
-- EMR cluster actions that their IAM policies allow. When @false@, only
-- the IAM principal that created the cluster and the Amazon Web Services
-- account root user can perform EMR actions on the cluster, regardless of
-- IAM permissions policies attached to other IAM principals.
--
-- This action works on running clusters. When you create a cluster, use
-- the RunJobFlowInput$VisibleToAllUsers parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
-- in the /Amazon EMRManagement Guide/.
module Amazonka.EMR.SetVisibleToAllUsers
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the SetVisibleToAllUsers action.
--
-- /See:/ 'newSetVisibleToAllUsers' smart constructor.
data SetVisibleToAllUsers = SetVisibleToAllUsers'
  { -- | The unique identifier of the job flow (cluster).
    jobFlowIds :: [Prelude.Text],
    -- | A value of @true@ indicates that an IAM principal in the Amazon Web
    -- Services account can perform EMR actions on the cluster that the IAM
    -- policies attached to the principal allow. A value of @false@ indicates
    -- that only the IAM principal that created the cluster and the Amazon Web
    -- Services root user can perform EMR actions on the cluster.
    visibleToAllUsers :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'visibleToAllUsers', 'setVisibleToAllUsers_visibleToAllUsers' - A value of @true@ indicates that an IAM principal in the Amazon Web
-- Services account can perform EMR actions on the cluster that the IAM
-- policies attached to the principal allow. A value of @false@ indicates
-- that only the IAM principal that created the cluster and the Amazon Web
-- Services root user can perform EMR actions on the cluster.
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
setVisibleToAllUsers_jobFlowIds = Lens.lens (\SetVisibleToAllUsers' {jobFlowIds} -> jobFlowIds) (\s@SetVisibleToAllUsers' {} a -> s {jobFlowIds = a} :: SetVisibleToAllUsers) Prelude.. Lens.coerced

-- | A value of @true@ indicates that an IAM principal in the Amazon Web
-- Services account can perform EMR actions on the cluster that the IAM
-- policies attached to the principal allow. A value of @false@ indicates
-- that only the IAM principal that created the cluster and the Amazon Web
-- Services root user can perform EMR actions on the cluster.
setVisibleToAllUsers_visibleToAllUsers :: Lens.Lens' SetVisibleToAllUsers Prelude.Bool
setVisibleToAllUsers_visibleToAllUsers = Lens.lens (\SetVisibleToAllUsers' {visibleToAllUsers} -> visibleToAllUsers) (\s@SetVisibleToAllUsers' {} a -> s {visibleToAllUsers = a} :: SetVisibleToAllUsers)

instance Core.AWSRequest SetVisibleToAllUsers where
  type
    AWSResponse SetVisibleToAllUsers =
      SetVisibleToAllUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull SetVisibleToAllUsersResponse'

instance Prelude.Hashable SetVisibleToAllUsers where
  hashWithSalt _salt SetVisibleToAllUsers' {..} =
    _salt
      `Prelude.hashWithSalt` jobFlowIds
      `Prelude.hashWithSalt` visibleToAllUsers

instance Prelude.NFData SetVisibleToAllUsers where
  rnf SetVisibleToAllUsers' {..} =
    Prelude.rnf jobFlowIds `Prelude.seq`
      Prelude.rnf visibleToAllUsers

instance Data.ToHeaders SetVisibleToAllUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.SetVisibleToAllUsers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetVisibleToAllUsers where
  toJSON SetVisibleToAllUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("JobFlowIds" Data..= jobFlowIds),
            Prelude.Just
              ("VisibleToAllUsers" Data..= visibleToAllUsers)
          ]
      )

instance Data.ToPath SetVisibleToAllUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery SetVisibleToAllUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetVisibleToAllUsersResponse' smart constructor.
data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetVisibleToAllUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetVisibleToAllUsersResponse ::
  SetVisibleToAllUsersResponse
newSetVisibleToAllUsersResponse =
  SetVisibleToAllUsersResponse'

instance Prelude.NFData SetVisibleToAllUsersResponse where
  rnf _ = ()
