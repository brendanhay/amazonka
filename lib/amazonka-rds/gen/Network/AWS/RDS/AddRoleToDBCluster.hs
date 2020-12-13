{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddRoleToDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Identity and Access Management (IAM) role from an Amazon Aurora DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Integrating.Authorizing.html Authorizing Amazon Aurora MySQL to Access Other AWS Services on Your Behalf> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.AddRoleToDBCluster
  ( -- * Creating a request
    AddRoleToDBCluster (..),
    mkAddRoleToDBCluster,

    -- ** Request lenses
    artdcDBClusterIdentifier,
    artdcFeatureName,
    artdcRoleARN,

    -- * Destructuring the response
    AddRoleToDBClusterResponse (..),
    mkAddRoleToDBClusterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddRoleToDBCluster' smart constructor.
data AddRoleToDBCluster = AddRoleToDBCluster'
  { -- | The name of the DB cluster to associate the IAM role with.
    dbClusterIdentifier :: Lude.Text,
    -- | The name of the feature for the DB cluster that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
    featureName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddRoleToDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The name of the DB cluster to associate the IAM role with.
-- * 'featureName' - The name of the feature for the DB cluster that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
mkAddRoleToDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AddRoleToDBCluster
mkAddRoleToDBCluster pDBClusterIdentifier_ pRoleARN_ =
  AddRoleToDBCluster'
    { dbClusterIdentifier = pDBClusterIdentifier_,
      featureName = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The name of the DB cluster to associate the IAM role with.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdcDBClusterIdentifier :: Lens.Lens' AddRoleToDBCluster Lude.Text
artdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: AddRoleToDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: AddRoleToDBCluster)
{-# DEPRECATED artdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The name of the feature for the DB cluster that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdcFeatureName :: Lens.Lens' AddRoleToDBCluster (Lude.Maybe Lude.Text)
artdcFeatureName = Lens.lens (featureName :: AddRoleToDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {featureName = a} :: AddRoleToDBCluster)
{-# DEPRECATED artdcFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdcRoleARN :: Lens.Lens' AddRoleToDBCluster Lude.Text
artdcRoleARN = Lens.lens (roleARN :: AddRoleToDBCluster -> Lude.Text) (\s a -> s {roleARN = a} :: AddRoleToDBCluster)
{-# DEPRECATED artdcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AddRoleToDBCluster where
  type Rs AddRoleToDBCluster = AddRoleToDBClusterResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull AddRoleToDBClusterResponse'

instance Lude.ToHeaders AddRoleToDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddRoleToDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery AddRoleToDBCluster where
  toQuery AddRoleToDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddRoleToDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "FeatureName" Lude.=: featureName,
        "RoleArn" Lude.=: roleARN
      ]

-- | /See:/ 'mkAddRoleToDBClusterResponse' smart constructor.
data AddRoleToDBClusterResponse = AddRoleToDBClusterResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddRoleToDBClusterResponse' with the minimum fields required to make a request.
mkAddRoleToDBClusterResponse ::
  AddRoleToDBClusterResponse
mkAddRoleToDBClusterResponse = AddRoleToDBClusterResponse'
