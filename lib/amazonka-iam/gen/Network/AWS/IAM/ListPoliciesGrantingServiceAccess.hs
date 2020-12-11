{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPoliciesGrantingServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of policies that the IAM identity (user, group, or role) can use to access each specified service.
--
-- The list of policies returned by the operation depends on the ARN of the identity that you provide.
--
--     * __User__ – The list of policies includes the managed and inline policies that are attached to the user directly. The list also includes any additional managed and inline policies that are attached to the group to which the user belongs.
--
--
--     * __Group__ – The list of policies includes only the managed and inline policies that are attached to the group directly. Policies that are attached to the group’s user are not included.
--
--
--     * __Role__ – The list of policies includes only the managed and inline policies that are attached to the role.
--
--
-- For each managed policy, this operation returns the ARN and policy name. For each inline policy, it returns the policy name and the entity to which it is attached. Inline policies do not have an ARN. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- Policies that are attached to users and roles as permissions boundaries are not returned. To view which managed policy is currently used to set the permissions boundary for a user or role, use the 'GetUser' or 'GetRole' operations.
module Network.AWS.IAM.ListPoliciesGrantingServiceAccess
  ( -- * Creating a request
    ListPoliciesGrantingServiceAccess (..),
    mkListPoliciesGrantingServiceAccess,

    -- ** Request lenses
    lpgsaMarker,
    lpgsaARN,
    lpgsaServiceNamespaces,

    -- * Destructuring the response
    ListPoliciesGrantingServiceAccessResponse (..),
    mkListPoliciesGrantingServiceAccessResponse,

    -- ** Response lenses
    lpgsarsMarker,
    lpgsarsIsTruncated,
    lpgsarsResponseStatus,
    lpgsarsPoliciesGrantingServiceAccess,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPoliciesGrantingServiceAccess' smart constructor.
data ListPoliciesGrantingServiceAccess = ListPoliciesGrantingServiceAccess'
  { marker ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Text,
    serviceNamespaces ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesGrantingServiceAccess' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the IAM identity (user, group, or role) whose policies you want to list.
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'serviceNamespaces' - The service namespace for the AWS services whose policies you want to list.
--
-- To learn the service namespace for a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
mkListPoliciesGrantingServiceAccess ::
  -- | 'arn'
  Lude.Text ->
  -- | 'serviceNamespaces'
  Lude.NonEmpty Lude.Text ->
  ListPoliciesGrantingServiceAccess
mkListPoliciesGrantingServiceAccess pARN_ pServiceNamespaces_ =
  ListPoliciesGrantingServiceAccess'
    { marker = Lude.Nothing,
      arn = pARN_,
      serviceNamespaces = pServiceNamespaces_
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaMarker :: Lens.Lens' ListPoliciesGrantingServiceAccess (Lude.Maybe Lude.Text)
lpgsaMarker = Lens.lens (marker :: ListPoliciesGrantingServiceAccess -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPoliciesGrantingServiceAccess)
{-# DEPRECATED lpgsaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The ARN of the IAM identity (user, group, or role) whose policies you want to list.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaARN :: Lens.Lens' ListPoliciesGrantingServiceAccess Lude.Text
lpgsaARN = Lens.lens (arn :: ListPoliciesGrantingServiceAccess -> Lude.Text) (\s a -> s {arn = a} :: ListPoliciesGrantingServiceAccess)
{-# DEPRECATED lpgsaARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The service namespace for the AWS services whose policies you want to list.
--
-- To learn the service namespace for a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaServiceNamespaces :: Lens.Lens' ListPoliciesGrantingServiceAccess (Lude.NonEmpty Lude.Text)
lpgsaServiceNamespaces = Lens.lens (serviceNamespaces :: ListPoliciesGrantingServiceAccess -> Lude.NonEmpty Lude.Text) (\s a -> s {serviceNamespaces = a} :: ListPoliciesGrantingServiceAccess)
{-# DEPRECATED lpgsaServiceNamespaces "Use generic-lens or generic-optics with 'serviceNamespaces' instead." #-}

instance Lude.AWSRequest ListPoliciesGrantingServiceAccess where
  type
    Rs ListPoliciesGrantingServiceAccess =
      ListPoliciesGrantingServiceAccessResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListPoliciesGrantingServiceAccessResult"
      ( \s h x ->
          ListPoliciesGrantingServiceAccessResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "PoliciesGrantingServiceAccess" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListPoliciesGrantingServiceAccess where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPoliciesGrantingServiceAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPoliciesGrantingServiceAccess where
  toQuery ListPoliciesGrantingServiceAccess' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ListPoliciesGrantingServiceAccess" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "Arn" Lude.=: arn,
        "ServiceNamespaces"
          Lude.=: Lude.toQueryList "member" serviceNamespaces
      ]

-- | /See:/ 'mkListPoliciesGrantingServiceAccessResponse' smart constructor.
data ListPoliciesGrantingServiceAccessResponse = ListPoliciesGrantingServiceAccessResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    isTruncated ::
      Lude.Maybe
        Lude.Bool,
    responseStatus ::
      Lude.Int,
    policiesGrantingServiceAccess ::
      [ListPoliciesGrantingServiceAccessEntry]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesGrantingServiceAccessResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'policiesGrantingServiceAccess' - A @ListPoliciesGrantingServiceAccess@ object that contains details about the permissions policies attached to the specified identity (user, group, or role).
-- * 'responseStatus' - The response status code.
mkListPoliciesGrantingServiceAccessResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPoliciesGrantingServiceAccessResponse
mkListPoliciesGrantingServiceAccessResponse pResponseStatus_ =
  ListPoliciesGrantingServiceAccessResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      policiesGrantingServiceAccess = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsarsMarker :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse (Lude.Maybe Lude.Text)
lpgsarsMarker = Lens.lens (marker :: ListPoliciesGrantingServiceAccessResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPoliciesGrantingServiceAccessResponse)
{-# DEPRECATED lpgsarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsarsIsTruncated :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse (Lude.Maybe Lude.Bool)
lpgsarsIsTruncated = Lens.lens (isTruncated :: ListPoliciesGrantingServiceAccessResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListPoliciesGrantingServiceAccessResponse)
{-# DEPRECATED lpgsarsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsarsResponseStatus :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse Lude.Int
lpgsarsResponseStatus = Lens.lens (responseStatus :: ListPoliciesGrantingServiceAccessResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPoliciesGrantingServiceAccessResponse)
{-# DEPRECATED lpgsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A @ListPoliciesGrantingServiceAccess@ object that contains details about the permissions policies attached to the specified identity (user, group, or role).
--
-- /Note:/ Consider using 'policiesGrantingServiceAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsarsPoliciesGrantingServiceAccess :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse [ListPoliciesGrantingServiceAccessEntry]
lpgsarsPoliciesGrantingServiceAccess = Lens.lens (policiesGrantingServiceAccess :: ListPoliciesGrantingServiceAccessResponse -> [ListPoliciesGrantingServiceAccessEntry]) (\s a -> s {policiesGrantingServiceAccess = a} :: ListPoliciesGrantingServiceAccessResponse)
{-# DEPRECATED lpgsarsPoliciesGrantingServiceAccess "Use generic-lens or generic-optics with 'policiesGrantingServiceAccess' instead." #-}
