{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    rrfdbcDBClusterIdentifier,
    rrfdbcRoleArn,
    rrfdbcFeatureName,

    -- * Destructuring the response
    RemoveRoleFromDBClusterResponse (..),
    mkRemoveRoleFromDBClusterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveRoleFromDBCluster' smart constructor.
data RemoveRoleFromDBCluster = RemoveRoleFromDBCluster'
  { -- | The name of the DB cluster to disassociate the IAM role from.
    dBClusterIdentifier :: Types.String,
    -- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
    roleArn :: Types.String,
    -- | The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
    featureName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromDBCluster' value with any optional fields omitted.
mkRemoveRoleFromDBCluster ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'roleArn'
  Types.String ->
  RemoveRoleFromDBCluster
mkRemoveRoleFromDBCluster dBClusterIdentifier roleArn =
  RemoveRoleFromDBCluster'
    { dBClusterIdentifier,
      roleArn,
      featureName = Core.Nothing
    }

-- | The name of the DB cluster to disassociate the IAM role from.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbcDBClusterIdentifier :: Lens.Lens' RemoveRoleFromDBCluster Types.String
rrfdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED rrfdbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbcRoleArn :: Lens.Lens' RemoveRoleFromDBCluster Types.String
rrfdbcRoleArn = Lens.field @"roleArn"
{-# DEPRECATED rrfdbcRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbcFeatureName :: Lens.Lens' RemoveRoleFromDBCluster (Core.Maybe Types.String)
rrfdbcFeatureName = Lens.field @"featureName"
{-# DEPRECATED rrfdbcFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

instance Core.AWSRequest RemoveRoleFromDBCluster where
  type Rs RemoveRoleFromDBCluster = RemoveRoleFromDBClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RemoveRoleFromDBCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "RoleArn" roleArn)
                Core.<> (Core.toQueryValue "FeatureName" Core.<$> featureName)
            )
      }
  response = Response.receiveNull RemoveRoleFromDBClusterResponse'

-- | /See:/ 'mkRemoveRoleFromDBClusterResponse' smart constructor.
data RemoveRoleFromDBClusterResponse = RemoveRoleFromDBClusterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromDBClusterResponse' value with any optional fields omitted.
mkRemoveRoleFromDBClusterResponse ::
  RemoveRoleFromDBClusterResponse
mkRemoveRoleFromDBClusterResponse =
  RemoveRoleFromDBClusterResponse'
