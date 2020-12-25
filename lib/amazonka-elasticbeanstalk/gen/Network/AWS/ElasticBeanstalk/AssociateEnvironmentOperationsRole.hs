{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or change the operations role used by an environment. After this call is made, Elastic Beanstalk uses the associated operations role for permissions to downstream services during subsequent calls acting on this environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
module Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
  ( -- * Creating a request
    AssociateEnvironmentOperationsRole (..),
    mkAssociateEnvironmentOperationsRole,

    -- ** Request lenses
    aeorEnvironmentName,
    aeorOperationsRole,

    -- * Destructuring the response
    AssociateEnvironmentOperationsRoleResponse (..),
    mkAssociateEnvironmentOperationsRoleResponse,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add or change the operations role used by an environment.
--
-- /See:/ 'mkAssociateEnvironmentOperationsRole' smart constructor.
data AssociateEnvironmentOperationsRole = AssociateEnvironmentOperationsRole'
  { -- | The name of the environment to which to set the operations role.
    environmentName :: Types.EnvironmentName,
    -- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role.
    operationsRole :: Types.OperationsRole
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateEnvironmentOperationsRole' value with any optional fields omitted.
mkAssociateEnvironmentOperationsRole ::
  -- | 'environmentName'
  Types.EnvironmentName ->
  -- | 'operationsRole'
  Types.OperationsRole ->
  AssociateEnvironmentOperationsRole
mkAssociateEnvironmentOperationsRole environmentName operationsRole =
  AssociateEnvironmentOperationsRole'
    { environmentName,
      operationsRole
    }

-- | The name of the environment to which to set the operations role.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeorEnvironmentName :: Lens.Lens' AssociateEnvironmentOperationsRole Types.EnvironmentName
aeorEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED aeorEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role.
--
-- /Note:/ Consider using 'operationsRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeorOperationsRole :: Lens.Lens' AssociateEnvironmentOperationsRole Types.OperationsRole
aeorOperationsRole = Lens.field @"operationsRole"
{-# DEPRECATED aeorOperationsRole "Use generic-lens or generic-optics with 'operationsRole' instead." #-}

instance Core.AWSRequest AssociateEnvironmentOperationsRole where
  type
    Rs AssociateEnvironmentOperationsRole =
      AssociateEnvironmentOperationsRoleResponse
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
            ( Core.pure ("Action", "AssociateEnvironmentOperationsRole")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "EnvironmentName" environmentName)
                Core.<> (Core.toQueryValue "OperationsRole" operationsRole)
            )
      }
  response =
    Response.receiveNull AssociateEnvironmentOperationsRoleResponse'

-- | /See:/ 'mkAssociateEnvironmentOperationsRoleResponse' smart constructor.
data AssociateEnvironmentOperationsRoleResponse = AssociateEnvironmentOperationsRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateEnvironmentOperationsRoleResponse' value with any optional fields omitted.
mkAssociateEnvironmentOperationsRoleResponse ::
  AssociateEnvironmentOperationsRoleResponse
mkAssociateEnvironmentOperationsRoleResponse =
  AssociateEnvironmentOperationsRoleResponse'
