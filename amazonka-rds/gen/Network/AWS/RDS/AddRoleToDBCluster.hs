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
-- Module      : Network.AWS.RDS.AddRoleToDBCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Identity and Access Management (IAM) role from an Amazon
-- Aurora DB cluster. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Integrating.Authorizing.html Authorizing Amazon Aurora MySQL to Access Other AWS Services on Your Behalf>
-- in the /Amazon Aurora User Guide/.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.AddRoleToDBCluster
  ( -- * Creating a Request
    AddRoleToDBCluster (..),
    newAddRoleToDBCluster,

    -- * Request Lenses
    addRoleToDBCluster_featureName,
    addRoleToDBCluster_dbClusterIdentifier,
    addRoleToDBCluster_roleArn,

    -- * Destructuring the Response
    AddRoleToDBClusterResponse (..),
    newAddRoleToDBClusterResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddRoleToDBCluster' smart constructor.
data AddRoleToDBCluster = AddRoleToDBCluster'
  { -- | The name of the feature for the DB cluster that the IAM role is to be
    -- associated with. For the list of supported feature names, see
    -- DBEngineVersion.
    featureName :: Core.Maybe Core.Text,
    -- | The name of the DB cluster to associate the IAM role with.
    dbClusterIdentifier :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the
    -- Aurora DB cluster, for example,
    -- @arn:aws:iam::123456789012:role\/AuroraAccessRole@.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddRoleToDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureName', 'addRoleToDBCluster_featureName' - The name of the feature for the DB cluster that the IAM role is to be
-- associated with. For the list of supported feature names, see
-- DBEngineVersion.
--
-- 'dbClusterIdentifier', 'addRoleToDBCluster_dbClusterIdentifier' - The name of the DB cluster to associate the IAM role with.
--
-- 'roleArn', 'addRoleToDBCluster_roleArn' - The Amazon Resource Name (ARN) of the IAM role to associate with the
-- Aurora DB cluster, for example,
-- @arn:aws:iam::123456789012:role\/AuroraAccessRole@.
newAddRoleToDBCluster ::
  -- | 'dbClusterIdentifier'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  AddRoleToDBCluster
newAddRoleToDBCluster pDBClusterIdentifier_ pRoleArn_ =
  AddRoleToDBCluster'
    { featureName = Core.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      roleArn = pRoleArn_
    }

-- | The name of the feature for the DB cluster that the IAM role is to be
-- associated with. For the list of supported feature names, see
-- DBEngineVersion.
addRoleToDBCluster_featureName :: Lens.Lens' AddRoleToDBCluster (Core.Maybe Core.Text)
addRoleToDBCluster_featureName = Lens.lens (\AddRoleToDBCluster' {featureName} -> featureName) (\s@AddRoleToDBCluster' {} a -> s {featureName = a} :: AddRoleToDBCluster)

-- | The name of the DB cluster to associate the IAM role with.
addRoleToDBCluster_dbClusterIdentifier :: Lens.Lens' AddRoleToDBCluster Core.Text
addRoleToDBCluster_dbClusterIdentifier = Lens.lens (\AddRoleToDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@AddRoleToDBCluster' {} a -> s {dbClusterIdentifier = a} :: AddRoleToDBCluster)

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the
-- Aurora DB cluster, for example,
-- @arn:aws:iam::123456789012:role\/AuroraAccessRole@.
addRoleToDBCluster_roleArn :: Lens.Lens' AddRoleToDBCluster Core.Text
addRoleToDBCluster_roleArn = Lens.lens (\AddRoleToDBCluster' {roleArn} -> roleArn) (\s@AddRoleToDBCluster' {} a -> s {roleArn = a} :: AddRoleToDBCluster)

instance Core.AWSRequest AddRoleToDBCluster where
  type
    AWSResponse AddRoleToDBCluster =
      AddRoleToDBClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddRoleToDBClusterResponse'

instance Core.Hashable AddRoleToDBCluster

instance Core.NFData AddRoleToDBCluster

instance Core.ToHeaders AddRoleToDBCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddRoleToDBCluster where
  toPath = Core.const "/"

instance Core.ToQuery AddRoleToDBCluster where
  toQuery AddRoleToDBCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AddRoleToDBCluster" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "FeatureName" Core.=: featureName,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "RoleArn" Core.=: roleArn
      ]

-- | /See:/ 'newAddRoleToDBClusterResponse' smart constructor.
data AddRoleToDBClusterResponse = AddRoleToDBClusterResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddRoleToDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddRoleToDBClusterResponse ::
  AddRoleToDBClusterResponse
newAddRoleToDBClusterResponse =
  AddRoleToDBClusterResponse'

instance Core.NFData AddRoleToDBClusterResponse
