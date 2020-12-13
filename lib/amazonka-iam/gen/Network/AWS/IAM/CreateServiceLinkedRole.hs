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
    cslrCustomSuffix,
    cslrAWSServiceName,
    cslrDescription,

    -- * Destructuring the response
    CreateServiceLinkedRoleResponse (..),
    mkCreateServiceLinkedRoleResponse,

    -- ** Response lenses
    cslrrsRole,
    cslrrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateServiceLinkedRole' smart constructor.
data CreateServiceLinkedRole = CreateServiceLinkedRole'
  { -- |
    --
    -- A string that you provide, which is combined with the service-provided prefix to form the complete role name. If you make multiple requests for the same service, then you must supply a different @CustomSuffix@ for each request. Otherwise the request fails with a duplicate role name error. For example, you could add @-1@ or @-debug@ to the suffix.
    -- Some services do not support the @CustomSuffix@ parameter. If you provide an optional suffix and the operation fails, try the operation again without the suffix.
    customSuffix :: Lude.Maybe Lude.Text,
    -- | The service principal for the AWS service to which this role is attached. You use a string similar to a URL but without the http:// in front. For example: @elasticbeanstalk.amazonaws.com@ .
    --
    -- Service principals are unique and case-sensitive. To find the exact service principal for your service-linked role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ . Look for the services that have __Yes __ in the __Service-Linked Role__ column. Choose the __Yes__ link to view the service-linked role documentation for that service.
    awsServiceName :: Lude.Text,
    -- | The description of the role.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServiceLinkedRole' with the minimum fields required to make a request.
--
-- * 'customSuffix' -
--
-- A string that you provide, which is combined with the service-provided prefix to form the complete role name. If you make multiple requests for the same service, then you must supply a different @CustomSuffix@ for each request. Otherwise the request fails with a duplicate role name error. For example, you could add @-1@ or @-debug@ to the suffix.
-- Some services do not support the @CustomSuffix@ parameter. If you provide an optional suffix and the operation fails, try the operation again without the suffix.
-- * 'awsServiceName' - The service principal for the AWS service to which this role is attached. You use a string similar to a URL but without the http:// in front. For example: @elasticbeanstalk.amazonaws.com@ .
--
-- Service principals are unique and case-sensitive. To find the exact service principal for your service-linked role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ . Look for the services that have __Yes __ in the __Service-Linked Role__ column. Choose the __Yes__ link to view the service-linked role documentation for that service.
-- * 'description' - The description of the role.
mkCreateServiceLinkedRole ::
  -- | 'awsServiceName'
  Lude.Text ->
  CreateServiceLinkedRole
mkCreateServiceLinkedRole pAWSServiceName_ =
  CreateServiceLinkedRole'
    { customSuffix = Lude.Nothing,
      awsServiceName = pAWSServiceName_,
      description = Lude.Nothing
    }

-- |
--
-- A string that you provide, which is combined with the service-provided prefix to form the complete role name. If you make multiple requests for the same service, then you must supply a different @CustomSuffix@ for each request. Otherwise the request fails with a duplicate role name error. For example, you could add @-1@ or @-debug@ to the suffix.
-- Some services do not support the @CustomSuffix@ parameter. If you provide an optional suffix and the operation fails, try the operation again without the suffix.
--
-- /Note:/ Consider using 'customSuffix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrCustomSuffix :: Lens.Lens' CreateServiceLinkedRole (Lude.Maybe Lude.Text)
cslrCustomSuffix = Lens.lens (customSuffix :: CreateServiceLinkedRole -> Lude.Maybe Lude.Text) (\s a -> s {customSuffix = a} :: CreateServiceLinkedRole)
{-# DEPRECATED cslrCustomSuffix "Use generic-lens or generic-optics with 'customSuffix' instead." #-}

-- | The service principal for the AWS service to which this role is attached. You use a string similar to a URL but without the http:// in front. For example: @elasticbeanstalk.amazonaws.com@ .
--
-- Service principals are unique and case-sensitive. To find the exact service principal for your service-linked role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ . Look for the services that have __Yes __ in the __Service-Linked Role__ column. Choose the __Yes__ link to view the service-linked role documentation for that service.
--
-- /Note:/ Consider using 'awsServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrAWSServiceName :: Lens.Lens' CreateServiceLinkedRole Lude.Text
cslrAWSServiceName = Lens.lens (awsServiceName :: CreateServiceLinkedRole -> Lude.Text) (\s a -> s {awsServiceName = a} :: CreateServiceLinkedRole)
{-# DEPRECATED cslrAWSServiceName "Use generic-lens or generic-optics with 'awsServiceName' instead." #-}

-- | The description of the role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrDescription :: Lens.Lens' CreateServiceLinkedRole (Lude.Maybe Lude.Text)
cslrDescription = Lens.lens (description :: CreateServiceLinkedRole -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateServiceLinkedRole)
{-# DEPRECATED cslrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateServiceLinkedRole where
  type Rs CreateServiceLinkedRole = CreateServiceLinkedRoleResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateServiceLinkedRoleResult"
      ( \s h x ->
          CreateServiceLinkedRoleResponse'
            Lude.<$> (x Lude..@? "Role") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateServiceLinkedRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateServiceLinkedRole where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateServiceLinkedRole where
  toQuery CreateServiceLinkedRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateServiceLinkedRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "CustomSuffix" Lude.=: customSuffix,
        "AWSServiceName" Lude.=: awsServiceName,
        "Description" Lude.=: description
      ]

-- | /See:/ 'mkCreateServiceLinkedRoleResponse' smart constructor.
data CreateServiceLinkedRoleResponse = CreateServiceLinkedRoleResponse'
  { -- | A 'Role' object that contains details about the newly created role.
    role' :: Lude.Maybe Role,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServiceLinkedRoleResponse' with the minimum fields required to make a request.
--
-- * 'role'' - A 'Role' object that contains details about the newly created role.
-- * 'responseStatus' - The response status code.
mkCreateServiceLinkedRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateServiceLinkedRoleResponse
mkCreateServiceLinkedRoleResponse pResponseStatus_ =
  CreateServiceLinkedRoleResponse'
    { role' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'Role' object that contains details about the newly created role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrrsRole :: Lens.Lens' CreateServiceLinkedRoleResponse (Lude.Maybe Role)
cslrrsRole = Lens.lens (role' :: CreateServiceLinkedRoleResponse -> Lude.Maybe Role) (\s a -> s {role' = a} :: CreateServiceLinkedRoleResponse)
{-# DEPRECATED cslrrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cslrrsResponseStatus :: Lens.Lens' CreateServiceLinkedRoleResponse Lude.Int
cslrrsResponseStatus = Lens.lens (responseStatus :: CreateServiceLinkedRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateServiceLinkedRoleResponse)
{-# DEPRECATED cslrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
