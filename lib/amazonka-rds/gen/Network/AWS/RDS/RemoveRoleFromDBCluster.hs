{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RemoveRoleFromDBCluster (..)
    , mkRemoveRoleFromDBCluster
    -- ** Request lenses
    , rrfdbcDBClusterIdentifier
    , rrfdbcRoleArn
    , rrfdbcFeatureName

    -- * Destructuring the response
    , RemoveRoleFromDBClusterResponse (..)
    , mkRemoveRoleFromDBClusterResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveRoleFromDBCluster' smart constructor.
data RemoveRoleFromDBCluster = RemoveRoleFromDBCluster'
  { dBClusterIdentifier :: Core.Text
    -- ^ The name of the DB cluster to disassociate the IAM role from.
  , roleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
  , featureName :: Core.Maybe Core.Text
    -- ^ The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromDBCluster' value with any optional fields omitted.
mkRemoveRoleFromDBCluster
    :: Core.Text -- ^ 'dBClusterIdentifier'
    -> Core.Text -- ^ 'roleArn'
    -> RemoveRoleFromDBCluster
mkRemoveRoleFromDBCluster dBClusterIdentifier roleArn
  = RemoveRoleFromDBCluster'{dBClusterIdentifier, roleArn,
                             featureName = Core.Nothing}

-- | The name of the DB cluster to disassociate the IAM role from.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbcDBClusterIdentifier :: Lens.Lens' RemoveRoleFromDBCluster Core.Text
rrfdbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE rrfdbcDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbcRoleArn :: Lens.Lens' RemoveRoleFromDBCluster Core.Text
rrfdbcRoleArn = Lens.field @"roleArn"
{-# INLINEABLE rrfdbcRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of the feature for the DB cluster that the IAM role is to be disassociated from. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdbcFeatureName :: Lens.Lens' RemoveRoleFromDBCluster (Core.Maybe Core.Text)
rrfdbcFeatureName = Lens.field @"featureName"
{-# INLINEABLE rrfdbcFeatureName #-}
{-# DEPRECATED featureName "Use generic-lens or generic-optics with 'featureName' instead"  #-}

instance Core.ToQuery RemoveRoleFromDBCluster where
        toQuery RemoveRoleFromDBCluster{..}
          = Core.toQueryPair "Action"
              ("RemoveRoleFromDBCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBClusterIdentifier" dBClusterIdentifier
              Core.<> Core.toQueryPair "RoleArn" roleArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FeatureName") featureName

instance Core.ToHeaders RemoveRoleFromDBCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RemoveRoleFromDBCluster where
        type Rs RemoveRoleFromDBCluster = RemoveRoleFromDBClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull RemoveRoleFromDBClusterResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveRoleFromDBClusterResponse' smart constructor.
data RemoveRoleFromDBClusterResponse = RemoveRoleFromDBClusterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveRoleFromDBClusterResponse' value with any optional fields omitted.
mkRemoveRoleFromDBClusterResponse
    :: RemoveRoleFromDBClusterResponse
mkRemoveRoleFromDBClusterResponse
  = RemoveRoleFromDBClusterResponse'
