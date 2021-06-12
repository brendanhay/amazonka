{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPoliciesGrantingServiceAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of policies that the IAM identity (user, group, or
-- role) can use to access each specified service.
--
-- This operation does not use other policy types when determining whether
-- a resource could access a service. These other policy types include
-- resource-based policies, access control lists, AWS Organizations
-- policies, IAM permissions boundaries, and AWS STS assume role policies.
-- It only applies permissions policy logic. For more about the evaluation
-- of policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-basics Evaluating policies>
-- in the /IAM User Guide/.
--
-- The list of policies returned by the operation depends on the ARN of the
-- identity that you provide.
--
-- -   __User__ – The list of policies includes the managed and inline
--     policies that are attached to the user directly. The list also
--     includes any additional managed and inline policies that are
--     attached to the group to which the user belongs.
--
-- -   __Group__ – The list of policies includes only the managed and
--     inline policies that are attached to the group directly. Policies
--     that are attached to the group’s user are not included.
--
-- -   __Role__ – The list of policies includes only the managed and inline
--     policies that are attached to the role.
--
-- For each managed policy, this operation returns the ARN and policy name.
-- For each inline policy, it returns the policy name and the entity to
-- which it is attached. Inline policies do not have an ARN. For more
-- information about these policy types, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- Policies that are attached to users and roles as permissions boundaries
-- are not returned. To view which managed policy is currently used to set
-- the permissions boundary for a user or role, use the GetUser or GetRole
-- operations.
module Network.AWS.IAM.ListPoliciesGrantingServiceAccess
  ( -- * Creating a Request
    ListPoliciesGrantingServiceAccess (..),
    newListPoliciesGrantingServiceAccess,

    -- * Request Lenses
    listPoliciesGrantingServiceAccess_marker,
    listPoliciesGrantingServiceAccess_arn,
    listPoliciesGrantingServiceAccess_serviceNamespaces,

    -- * Destructuring the Response
    ListPoliciesGrantingServiceAccessResponse (..),
    newListPoliciesGrantingServiceAccessResponse,

    -- * Response Lenses
    listPoliciesGrantingServiceAccessResponse_isTruncated,
    listPoliciesGrantingServiceAccessResponse_marker,
    listPoliciesGrantingServiceAccessResponse_httpStatus,
    listPoliciesGrantingServiceAccessResponse_policiesGrantingServiceAccess,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPoliciesGrantingServiceAccess' smart constructor.
data ListPoliciesGrantingServiceAccess = ListPoliciesGrantingServiceAccess'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Core.Maybe Core.Text,
    -- | The ARN of the IAM identity (user, group, or role) whose policies you
    -- want to list.
    arn :: Core.Text,
    -- | The service namespace for the AWS services whose policies you want to
    -- list.
    --
    -- To learn the service namespace for a service, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
    -- in the /IAM User Guide/. Choose the name of the service to view details
    -- for that service. In the first paragraph, find the service prefix. For
    -- example, @(service prefix: a4b)@. For more information about service
    -- namespaces, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS service namespaces>
    -- in the /AWS General Reference/.
    serviceNamespaces :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPoliciesGrantingServiceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPoliciesGrantingServiceAccess_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'arn', 'listPoliciesGrantingServiceAccess_arn' - The ARN of the IAM identity (user, group, or role) whose policies you
-- want to list.
--
-- 'serviceNamespaces', 'listPoliciesGrantingServiceAccess_serviceNamespaces' - The service namespace for the AWS services whose policies you want to
-- list.
--
-- To learn the service namespace for a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
-- in the /IAM User Guide/. Choose the name of the service to view details
-- for that service. In the first paragraph, find the service prefix. For
-- example, @(service prefix: a4b)@. For more information about service
-- namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS service namespaces>
-- in the /AWS General Reference/.
newListPoliciesGrantingServiceAccess ::
  -- | 'arn'
  Core.Text ->
  -- | 'serviceNamespaces'
  Core.NonEmpty Core.Text ->
  ListPoliciesGrantingServiceAccess
newListPoliciesGrantingServiceAccess
  pArn_
  pServiceNamespaces_ =
    ListPoliciesGrantingServiceAccess'
      { marker =
          Core.Nothing,
        arn = pArn_,
        serviceNamespaces =
          Lens._Coerce
            Lens.# pServiceNamespaces_
      }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listPoliciesGrantingServiceAccess_marker :: Lens.Lens' ListPoliciesGrantingServiceAccess (Core.Maybe Core.Text)
listPoliciesGrantingServiceAccess_marker = Lens.lens (\ListPoliciesGrantingServiceAccess' {marker} -> marker) (\s@ListPoliciesGrantingServiceAccess' {} a -> s {marker = a} :: ListPoliciesGrantingServiceAccess)

-- | The ARN of the IAM identity (user, group, or role) whose policies you
-- want to list.
listPoliciesGrantingServiceAccess_arn :: Lens.Lens' ListPoliciesGrantingServiceAccess Core.Text
listPoliciesGrantingServiceAccess_arn = Lens.lens (\ListPoliciesGrantingServiceAccess' {arn} -> arn) (\s@ListPoliciesGrantingServiceAccess' {} a -> s {arn = a} :: ListPoliciesGrantingServiceAccess)

-- | The service namespace for the AWS services whose policies you want to
-- list.
--
-- To learn the service namespace for a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
-- in the /IAM User Guide/. Choose the name of the service to view details
-- for that service. In the first paragraph, find the service prefix. For
-- example, @(service prefix: a4b)@. For more information about service
-- namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS service namespaces>
-- in the /AWS General Reference/.
listPoliciesGrantingServiceAccess_serviceNamespaces :: Lens.Lens' ListPoliciesGrantingServiceAccess (Core.NonEmpty Core.Text)
listPoliciesGrantingServiceAccess_serviceNamespaces = Lens.lens (\ListPoliciesGrantingServiceAccess' {serviceNamespaces} -> serviceNamespaces) (\s@ListPoliciesGrantingServiceAccess' {} a -> s {serviceNamespaces = a} :: ListPoliciesGrantingServiceAccess) Core.. Lens._Coerce

instance
  Core.AWSRequest
    ListPoliciesGrantingServiceAccess
  where
  type
    AWSResponse ListPoliciesGrantingServiceAccess =
      ListPoliciesGrantingServiceAccessResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPoliciesGrantingServiceAccessResult"
      ( \s h x ->
          ListPoliciesGrantingServiceAccessResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "PoliciesGrantingServiceAccess"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance
  Core.Hashable
    ListPoliciesGrantingServiceAccess

instance
  Core.NFData
    ListPoliciesGrantingServiceAccess

instance
  Core.ToHeaders
    ListPoliciesGrantingServiceAccess
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ListPoliciesGrantingServiceAccess
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListPoliciesGrantingServiceAccess
  where
  toQuery ListPoliciesGrantingServiceAccess' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ListPoliciesGrantingServiceAccess" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "Marker" Core.=: marker,
        "Arn" Core.=: arn,
        "ServiceNamespaces"
          Core.=: Core.toQueryList "member" serviceNamespaces
      ]

-- | /See:/ 'newListPoliciesGrantingServiceAccessResponse' smart constructor.
data ListPoliciesGrantingServiceAccessResponse = ListPoliciesGrantingServiceAccessResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. We
    -- recommend that you check @IsTruncated@ after every call to ensure that
    -- you receive all your results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A @ListPoliciesGrantingServiceAccess@ object that contains details about
    -- the permissions policies attached to the specified identity (user,
    -- group, or role).
    policiesGrantingServiceAccess :: [ListPoliciesGrantingServiceAccessEntry]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPoliciesGrantingServiceAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listPoliciesGrantingServiceAccessResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. We
-- recommend that you check @IsTruncated@ after every call to ensure that
-- you receive all your results.
--
-- 'marker', 'listPoliciesGrantingServiceAccessResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listPoliciesGrantingServiceAccessResponse_httpStatus' - The response's http status code.
--
-- 'policiesGrantingServiceAccess', 'listPoliciesGrantingServiceAccessResponse_policiesGrantingServiceAccess' - A @ListPoliciesGrantingServiceAccess@ object that contains details about
-- the permissions policies attached to the specified identity (user,
-- group, or role).
newListPoliciesGrantingServiceAccessResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPoliciesGrantingServiceAccessResponse
newListPoliciesGrantingServiceAccessResponse
  pHttpStatus_ =
    ListPoliciesGrantingServiceAccessResponse'
      { isTruncated =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_,
        policiesGrantingServiceAccess =
          Core.mempty
      }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. We
-- recommend that you check @IsTruncated@ after every call to ensure that
-- you receive all your results.
listPoliciesGrantingServiceAccessResponse_isTruncated :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse (Core.Maybe Core.Bool)
listPoliciesGrantingServiceAccessResponse_isTruncated = Lens.lens (\ListPoliciesGrantingServiceAccessResponse' {isTruncated} -> isTruncated) (\s@ListPoliciesGrantingServiceAccessResponse' {} a -> s {isTruncated = a} :: ListPoliciesGrantingServiceAccessResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listPoliciesGrantingServiceAccessResponse_marker :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse (Core.Maybe Core.Text)
listPoliciesGrantingServiceAccessResponse_marker = Lens.lens (\ListPoliciesGrantingServiceAccessResponse' {marker} -> marker) (\s@ListPoliciesGrantingServiceAccessResponse' {} a -> s {marker = a} :: ListPoliciesGrantingServiceAccessResponse)

-- | The response's http status code.
listPoliciesGrantingServiceAccessResponse_httpStatus :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse Core.Int
listPoliciesGrantingServiceAccessResponse_httpStatus = Lens.lens (\ListPoliciesGrantingServiceAccessResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesGrantingServiceAccessResponse' {} a -> s {httpStatus = a} :: ListPoliciesGrantingServiceAccessResponse)

-- | A @ListPoliciesGrantingServiceAccess@ object that contains details about
-- the permissions policies attached to the specified identity (user,
-- group, or role).
listPoliciesGrantingServiceAccessResponse_policiesGrantingServiceAccess :: Lens.Lens' ListPoliciesGrantingServiceAccessResponse [ListPoliciesGrantingServiceAccessEntry]
listPoliciesGrantingServiceAccessResponse_policiesGrantingServiceAccess = Lens.lens (\ListPoliciesGrantingServiceAccessResponse' {policiesGrantingServiceAccess} -> policiesGrantingServiceAccess) (\s@ListPoliciesGrantingServiceAccessResponse' {} a -> s {policiesGrantingServiceAccess = a} :: ListPoliciesGrantingServiceAccessResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListPoliciesGrantingServiceAccessResponse
