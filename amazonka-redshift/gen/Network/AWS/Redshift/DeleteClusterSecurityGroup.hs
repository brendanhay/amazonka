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
-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift security group.
--
-- You cannot delete a security group that is associated with any clusters.
-- You cannot delete the default security group.
--
-- For information about managing security groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
module Network.AWS.Redshift.DeleteClusterSecurityGroup
  ( -- * Creating a Request
    DeleteClusterSecurityGroup (..),
    newDeleteClusterSecurityGroup,

    -- * Request Lenses
    deleteClusterSecurityGroup_clusterSecurityGroupName,

    -- * Destructuring the Response
    DeleteClusterSecurityGroupResponse (..),
    newDeleteClusterSecurityGroupResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteClusterSecurityGroup' smart constructor.
data DeleteClusterSecurityGroup = DeleteClusterSecurityGroup'
  { -- | The name of the cluster security group to be deleted.
    clusterSecurityGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterSecurityGroupName', 'deleteClusterSecurityGroup_clusterSecurityGroupName' - The name of the cluster security group to be deleted.
newDeleteClusterSecurityGroup ::
  -- | 'clusterSecurityGroupName'
  Prelude.Text ->
  DeleteClusterSecurityGroup
newDeleteClusterSecurityGroup
  pClusterSecurityGroupName_ =
    DeleteClusterSecurityGroup'
      { clusterSecurityGroupName =
          pClusterSecurityGroupName_
      }

-- | The name of the cluster security group to be deleted.
deleteClusterSecurityGroup_clusterSecurityGroupName :: Lens.Lens' DeleteClusterSecurityGroup Prelude.Text
deleteClusterSecurityGroup_clusterSecurityGroupName = Lens.lens (\DeleteClusterSecurityGroup' {clusterSecurityGroupName} -> clusterSecurityGroupName) (\s@DeleteClusterSecurityGroup' {} a -> s {clusterSecurityGroupName = a} :: DeleteClusterSecurityGroup)

instance Core.AWSRequest DeleteClusterSecurityGroup where
  type
    AWSResponse DeleteClusterSecurityGroup =
      DeleteClusterSecurityGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteClusterSecurityGroupResponse'

instance Prelude.Hashable DeleteClusterSecurityGroup

instance Prelude.NFData DeleteClusterSecurityGroup

instance Core.ToHeaders DeleteClusterSecurityGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteClusterSecurityGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteClusterSecurityGroup where
  toQuery DeleteClusterSecurityGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteClusterSecurityGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterSecurityGroupName"
          Core.=: clusterSecurityGroupName
      ]

-- | /See:/ 'newDeleteClusterSecurityGroupResponse' smart constructor.
data DeleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSecurityGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteClusterSecurityGroupResponse ::
  DeleteClusterSecurityGroupResponse
newDeleteClusterSecurityGroupResponse =
  DeleteClusterSecurityGroupResponse'

instance
  Prelude.NFData
    DeleteClusterSecurityGroupResponse
