{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift security group.
--
-- For information about managing security groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.DeleteClusterSecurityGroup
  ( -- * Creating a request
    DeleteClusterSecurityGroup (..),
    mkDeleteClusterSecurityGroup,

    -- ** Request lenses
    dClusterSecurityGroupName,

    -- * Destructuring the response
    DeleteClusterSecurityGroupResponse (..),
    mkDeleteClusterSecurityGroupResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteClusterSecurityGroup' smart constructor.
newtype DeleteClusterSecurityGroup = DeleteClusterSecurityGroup'
  { -- | The name of the cluster security group to be deleted.
    clusterSecurityGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSecurityGroup' value with any optional fields omitted.
mkDeleteClusterSecurityGroup ::
  -- | 'clusterSecurityGroupName'
  Types.String ->
  DeleteClusterSecurityGroup
mkDeleteClusterSecurityGroup clusterSecurityGroupName =
  DeleteClusterSecurityGroup' {clusterSecurityGroupName}

-- | The name of the cluster security group to be deleted.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClusterSecurityGroupName :: Lens.Lens' DeleteClusterSecurityGroup Types.String
dClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# DEPRECATED dClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

instance Core.AWSRequest DeleteClusterSecurityGroup where
  type
    Rs DeleteClusterSecurityGroup =
      DeleteClusterSecurityGroupResponse
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
            ( Core.pure ("Action", "DeleteClusterSecurityGroup")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue
                            "ClusterSecurityGroupName"
                            clusterSecurityGroupName
                        )
            )
      }
  response = Response.receiveNull DeleteClusterSecurityGroupResponse'

-- | /See:/ 'mkDeleteClusterSecurityGroupResponse' smart constructor.
data DeleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteClusterSecurityGroupResponse' value with any optional fields omitted.
mkDeleteClusterSecurityGroupResponse ::
  DeleteClusterSecurityGroupResponse
mkDeleteClusterSecurityGroupResponse =
  DeleteClusterSecurityGroupResponse'
