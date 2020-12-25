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
    artdbcDBClusterIdentifier,
    artdbcRoleArn,
    artdbcFeatureName,

    -- * Destructuring the response
    AddRoleToDBClusterResponse (..),
    mkAddRoleToDBClusterResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddRoleToDBCluster' smart constructor.
data AddRoleToDBCluster = AddRoleToDBCluster'
  { -- | The name of the DB cluster to associate the IAM role with.
    dBClusterIdentifier :: Types.String,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
    roleArn :: Types.String,
    -- | The name of the feature for the DB cluster that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
    featureName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToDBCluster' value with any optional fields omitted.
mkAddRoleToDBCluster ::
  -- | 'dBClusterIdentifier'
  Types.String ->
  -- | 'roleArn'
  Types.String ->
  AddRoleToDBCluster
mkAddRoleToDBCluster dBClusterIdentifier roleArn =
  AddRoleToDBCluster'
    { dBClusterIdentifier,
      roleArn,
      featureName = Core.Nothing
    }

-- | The name of the DB cluster to associate the IAM role with.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbcDBClusterIdentifier :: Lens.Lens' AddRoleToDBCluster Types.String
artdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED artdbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbcRoleArn :: Lens.Lens' AddRoleToDBCluster Types.String
artdbcRoleArn = Lens.field @"roleArn"
{-# DEPRECATED artdbcRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The name of the feature for the DB cluster that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdbcFeatureName :: Lens.Lens' AddRoleToDBCluster (Core.Maybe Types.String)
artdbcFeatureName = Lens.field @"featureName"
{-# DEPRECATED artdbcFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

instance Core.AWSRequest AddRoleToDBCluster where
  type Rs AddRoleToDBCluster = AddRoleToDBClusterResponse
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
            ( Core.pure ("Action", "AddRoleToDBCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "RoleArn" roleArn)
                Core.<> (Core.toQueryValue "FeatureName" Core.<$> featureName)
            )
      }
  response = Response.receiveNull AddRoleToDBClusterResponse'

-- | /See:/ 'mkAddRoleToDBClusterResponse' smart constructor.
data AddRoleToDBClusterResponse = AddRoleToDBClusterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRoleToDBClusterResponse' value with any optional fields omitted.
mkAddRoleToDBClusterResponse ::
  AddRoleToDBClusterResponse
mkAddRoleToDBClusterResponse = AddRoleToDBClusterResponse'
