{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateServiceLinkedRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM role that is linked to a specific AWS service. The service controls the attached policies and when the role can be deleted. This helps ensure that the service is not broken by an unexpectedly changed or deleted role, which could put your AWS resources into an unknown state. Allowing the service to control the role helps improve service stability and proper cleanup when a service and its role are no longer needed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html Using Service-Linked Roles> in the /IAM User Guide/ .
--
-- To attach a policy to this service-linked role, you must make the request using the AWS service that depends on this role.
module Network.AWS.IAM.CreateServiceLinkedRole
  ( -- * Creating a request
    CreateServiceLinkedRole (..),
    mkCreateServiceLinkedRole,

    -- ** Request lenses
    cslrAWSServiceName,
    cslrCustomSuffix,
    cslrDescription,

    -- * Destructuring the response
    CreateServiceLinkedRoleResponse (..),
    mkCreateServiceLinkedRoleResponse,

    -- ** Response lenses
    cslrrrsRole,
    cslrrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateServiceLinkedRole' smart constructor.
data CreateServiceLinkedRole = CreateServiceLinkedRole'
  { -- | The service principal for the AWS service to which this role is attached. You use a string similar to a URL but without the http:// in front. For example: @elasticbeanstalk.amazonaws.com@ .
    --
    -- Service principals are unique and case-sensitive. To find the exact service principal for your service-linked role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ . Look for the services that have __Yes __ in the __Service-Linked Role__ column. Choose the __Yes__ link to view the service-linked role documentation for that service.
    aWSServiceName :: Types.AWSServiceName,
    -- |
    --
    -- A string that you provide, which is combined with the service-provided prefix to form the complete role name. If you make multiple requests for the same service, then you must supply a different @CustomSuffix@ for each request. Otherwise the request fails with a duplicate role name error. For example, you could add @-1@ or @-debug@ to the suffix.
    -- Some services do not support the @CustomSuffix@ parameter. If you provide an optional suffix and the operation fails, try the operation again without the suffix.
    customSuffix :: Core.Maybe Types.CustomSuffix,
    -- | The description of the role.
    description :: Core.Maybe Types.RoleDescriptionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateServiceLinkedRole' value with any optional fields omitted.
mkCreateServiceLinkedRole ::
  -- | 'aWSServiceName'
  Types.AWSServiceName ->
  CreateServiceLinkedRole
mkCreateServiceLinkedRole aWSServiceName =
  CreateServiceLinkedRole'
    { aWSServiceName,
      customSuffix = Core.Nothing,
      description = Core.Nothing
    }

-- | The service principal for the AWS service to which this role is attached. You use a string similar to a URL but without the http:// in front. For example: @elasticbeanstalk.amazonaws.com@ .
--
-- Service principals are unique and case-sensitive. To find the exact service principal for your service-linked role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ . Look for the services that have __Yes __ in the __Service-Linked Role__ column. Choose the __Yes__ link to view the service-linked role documentation for that service.
--
-- /Note:/ Consider using 'aWSServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrAWSServiceName :: Lens.Lens' CreateServiceLinkedRole Types.AWSServiceName
cslrAWSServiceName = Lens.field @"aWSServiceName"
{-# DEPRECATED cslrAWSServiceName "Use generic-lens or generic-optics with 'aWSServiceName' instead." #-}

-- |
--
-- A string that you provide, which is combined with the service-provided prefix to form the complete role name. If you make multiple requests for the same service, then you must supply a different @CustomSuffix@ for each request. Otherwise the request fails with a duplicate role name error. For example, you could add @-1@ or @-debug@ to the suffix.
-- Some services do not support the @CustomSuffix@ parameter. If you provide an optional suffix and the operation fails, try the operation again without the suffix.
--
-- /Note:/ Consider using 'customSuffix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrCustomSuffix :: Lens.Lens' CreateServiceLinkedRole (Core.Maybe Types.CustomSuffix)
cslrCustomSuffix = Lens.field @"customSuffix"
{-# DEPRECATED cslrCustomSuffix "Use generic-lens or generic-optics with 'customSuffix' instead." #-}

-- | The description of the role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrDescription :: Lens.Lens' CreateServiceLinkedRole (Core.Maybe Types.RoleDescriptionType)
cslrDescription = Lens.field @"description"
{-# DEPRECATED cslrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.AWSRequest CreateServiceLinkedRole where
  type Rs CreateServiceLinkedRole = CreateServiceLinkedRoleResponse
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
            ( Core.pure ("Action", "CreateServiceLinkedRole")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "AWSServiceName" aWSServiceName)
                Core.<> (Core.toQueryValue "CustomSuffix" Core.<$> customSuffix)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateServiceLinkedRoleResult"
      ( \s h x ->
          CreateServiceLinkedRoleResponse'
            Core.<$> (x Core..@? "Role") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateServiceLinkedRoleResponse' smart constructor.
data CreateServiceLinkedRoleResponse = CreateServiceLinkedRoleResponse'
  { -- | A 'Role' object that contains details about the newly created role.
    role' :: Core.Maybe Types.Role,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateServiceLinkedRoleResponse' value with any optional fields omitted.
mkCreateServiceLinkedRoleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateServiceLinkedRoleResponse
mkCreateServiceLinkedRoleResponse responseStatus =
  CreateServiceLinkedRoleResponse'
    { role' = Core.Nothing,
      responseStatus
    }

-- | A 'Role' object that contains details about the newly created role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrrrsRole :: Lens.Lens' CreateServiceLinkedRoleResponse (Core.Maybe Types.Role)
cslrrrsRole = Lens.field @"role'"
{-# DEPRECATED cslrrrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrrrsResponseStatus :: Lens.Lens' CreateServiceLinkedRoleResponse Core.Int
cslrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cslrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
