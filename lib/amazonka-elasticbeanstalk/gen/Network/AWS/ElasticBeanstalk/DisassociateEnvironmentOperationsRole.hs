{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate the operations role from an environment. After this call is made, Elastic Beanstalk uses the caller's permissions for permissions to downstream services during subsequent calls acting on this environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
module Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
  ( -- * Creating a request
    DisassociateEnvironmentOperationsRole (..),
    mkDisassociateEnvironmentOperationsRole,

    -- ** Request lenses
    deorEnvironmentName,

    -- * Destructuring the response
    DisassociateEnvironmentOperationsRoleResponse (..),
    mkDisassociateEnvironmentOperationsRoleResponse,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to disassociate the operations role from an environment.
--
-- /See:/ 'mkDisassociateEnvironmentOperationsRole' smart constructor.
newtype DisassociateEnvironmentOperationsRole = DisassociateEnvironmentOperationsRole'
  { -- | The name of the environment from which to disassociate the operations role.
    environmentName :: Types.EnvironmentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateEnvironmentOperationsRole' value with any optional fields omitted.
mkDisassociateEnvironmentOperationsRole ::
  -- | 'environmentName'
  Types.EnvironmentName ->
  DisassociateEnvironmentOperationsRole
mkDisassociateEnvironmentOperationsRole environmentName =
  DisassociateEnvironmentOperationsRole' {environmentName}

-- | The name of the environment from which to disassociate the operations role.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deorEnvironmentName :: Lens.Lens' DisassociateEnvironmentOperationsRole Types.EnvironmentName
deorEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED deorEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Core.AWSRequest DisassociateEnvironmentOperationsRole where
  type
    Rs DisassociateEnvironmentOperationsRole =
      DisassociateEnvironmentOperationsRoleResponse
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
            ( Core.pure ("Action", "DisassociateEnvironmentOperationsRole")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "EnvironmentName" environmentName)
            )
      }
  response =
    Response.receiveNull
      DisassociateEnvironmentOperationsRoleResponse'

-- | /See:/ 'mkDisassociateEnvironmentOperationsRoleResponse' smart constructor.
data DisassociateEnvironmentOperationsRoleResponse = DisassociateEnvironmentOperationsRoleResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateEnvironmentOperationsRoleResponse' value with any optional fields omitted.
mkDisassociateEnvironmentOperationsRoleResponse ::
  DisassociateEnvironmentOperationsRoleResponse
mkDisassociateEnvironmentOperationsRoleResponse =
  DisassociateEnvironmentOperationsRoleResponse'
