{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveRoleFromDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an AWS Identity and Access Management (IAM) role from an Amazon Aurora DB cluster. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Integrating.Authorizing.html Authorizing Amazon Aurora MySQL to Access Other AWS Services on Your Behalf > in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.RemoveRoleFromDBCluster
  ( -- * Creating a request
    RemoveRoleFromDBCluster (..),
    mkRemoveRoleFromDBCluster,

    -- ** Request lenses
    rrfdcFeatureName,
    rrfdcDBClusterIdentifier,
    rrfdcRoleARN,

    -- * Destructuring the response
    RemoveRoleFromDBClusterResponse (..),
    mkRemoveRoleFromDBClusterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveRoleFromDBCluster' smart constructor.
data RemoveRoleFromDBCluster = RemoveRoleFromDBCluster'
  { featureName ::
      Lude.Maybe Lude.Text,
    dbClusterIdentifier :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRoleFromDBCluster' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The name of the DB cluster to disassociate the IAM role from.
-- * 'featureName' - The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
mkRemoveRoleFromDBCluster ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  RemoveRoleFromDBCluster
mkRemoveRoleFromDBCluster pDBClusterIdentifier_ pRoleARN_ =
  RemoveRoleFromDBCluster'
    { featureName = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      roleARN = pRoleARN_
    }

-- | The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdcFeatureName :: Lens.Lens' RemoveRoleFromDBCluster (Lude.Maybe Lude.Text)
rrfdcFeatureName = Lens.lens (featureName :: RemoveRoleFromDBCluster -> Lude.Maybe Lude.Text) (\s a -> s {featureName = a} :: RemoveRoleFromDBCluster)
{-# DEPRECATED rrfdcFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

-- | The name of the DB cluster to disassociate the IAM role from.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdcDBClusterIdentifier :: Lens.Lens' RemoveRoleFromDBCluster Lude.Text
rrfdcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: RemoveRoleFromDBCluster -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: RemoveRoleFromDBCluster)
{-# DEPRECATED rrfdcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdcRoleARN :: Lens.Lens' RemoveRoleFromDBCluster Lude.Text
rrfdcRoleARN = Lens.lens (roleARN :: RemoveRoleFromDBCluster -> Lude.Text) (\s a -> s {roleARN = a} :: RemoveRoleFromDBCluster)
{-# DEPRECATED rrfdcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest RemoveRoleFromDBCluster where
  type Rs RemoveRoleFromDBCluster = RemoveRoleFromDBClusterResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull RemoveRoleFromDBClusterResponse'

instance Lude.ToHeaders RemoveRoleFromDBCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveRoleFromDBCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveRoleFromDBCluster where
  toQuery RemoveRoleFromDBCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveRoleFromDBCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "FeatureName" Lude.=: featureName,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "RoleArn" Lude.=: roleARN
      ]

-- | /See:/ 'mkRemoveRoleFromDBClusterResponse' smart constructor.
data RemoveRoleFromDBClusterResponse = RemoveRoleFromDBClusterResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRoleFromDBClusterResponse' with the minimum fields required to make a request.
mkRemoveRoleFromDBClusterResponse ::
  RemoveRoleFromDBClusterResponse
mkRemoveRoleFromDBClusterResponse =
  RemoveRoleFromDBClusterResponse'
