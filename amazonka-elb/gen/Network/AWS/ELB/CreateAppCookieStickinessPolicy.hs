{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELB.CreateAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated
-- only with HTTP\/HTTPS listeners.
--
-- This policy is similar to the policy created by
-- CreateLBCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie, @AWSELB@, follows the lifetime of the
-- application-generated cookie specified in the policy configuration. The
-- load balancer only inserts a new stickiness cookie when the application
-- response includes a new application cookie.
--
-- If the application cookie is explicitly removed or expires, the session
-- stops being sticky until a new application cookie is issued.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application Application-Controlled Session Stickiness>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.CreateAppCookieStickinessPolicy
  ( -- * Creating a Request
    CreateAppCookieStickinessPolicy (..),
    newCreateAppCookieStickinessPolicy,

    -- * Request Lenses
    createAppCookieStickinessPolicy_loadBalancerName,
    createAppCookieStickinessPolicy_policyName,
    createAppCookieStickinessPolicy_cookieName,

    -- * Destructuring the Response
    CreateAppCookieStickinessPolicyResponse (..),
    newCreateAppCookieStickinessPolicyResponse,

    -- * Response Lenses
    createAppCookieStickinessPolicyResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateAppCookieStickinessPolicy.
--
-- /See:/ 'newCreateAppCookieStickinessPolicy' smart constructor.
data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The name of the policy being created. Policy names must consist of
    -- alphanumeric characters and dashes (-). This name must be unique within
    -- the set of policies for this load balancer.
    policyName :: Prelude.Text,
    -- | The name of the application cookie used for stickiness.
    cookieName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAppCookieStickinessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'createAppCookieStickinessPolicy_loadBalancerName' - The name of the load balancer.
--
-- 'policyName', 'createAppCookieStickinessPolicy_policyName' - The name of the policy being created. Policy names must consist of
-- alphanumeric characters and dashes (-). This name must be unique within
-- the set of policies for this load balancer.
--
-- 'cookieName', 'createAppCookieStickinessPolicy_cookieName' - The name of the application cookie used for stickiness.
newCreateAppCookieStickinessPolicy ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'cookieName'
  Prelude.Text ->
  CreateAppCookieStickinessPolicy
newCreateAppCookieStickinessPolicy
  pLoadBalancerName_
  pPolicyName_
  pCookieName_ =
    CreateAppCookieStickinessPolicy'
      { loadBalancerName =
          pLoadBalancerName_,
        policyName = pPolicyName_,
        cookieName = pCookieName_
      }

-- | The name of the load balancer.
createAppCookieStickinessPolicy_loadBalancerName :: Lens.Lens' CreateAppCookieStickinessPolicy Prelude.Text
createAppCookieStickinessPolicy_loadBalancerName = Lens.lens (\CreateAppCookieStickinessPolicy' {loadBalancerName} -> loadBalancerName) (\s@CreateAppCookieStickinessPolicy' {} a -> s {loadBalancerName = a} :: CreateAppCookieStickinessPolicy)

-- | The name of the policy being created. Policy names must consist of
-- alphanumeric characters and dashes (-). This name must be unique within
-- the set of policies for this load balancer.
createAppCookieStickinessPolicy_policyName :: Lens.Lens' CreateAppCookieStickinessPolicy Prelude.Text
createAppCookieStickinessPolicy_policyName = Lens.lens (\CreateAppCookieStickinessPolicy' {policyName} -> policyName) (\s@CreateAppCookieStickinessPolicy' {} a -> s {policyName = a} :: CreateAppCookieStickinessPolicy)

-- | The name of the application cookie used for stickiness.
createAppCookieStickinessPolicy_cookieName :: Lens.Lens' CreateAppCookieStickinessPolicy Prelude.Text
createAppCookieStickinessPolicy_cookieName = Lens.lens (\CreateAppCookieStickinessPolicy' {cookieName} -> cookieName) (\s@CreateAppCookieStickinessPolicy' {} a -> s {cookieName = a} :: CreateAppCookieStickinessPolicy)

instance
  Prelude.AWSRequest
    CreateAppCookieStickinessPolicy
  where
  type
    Rs CreateAppCookieStickinessPolicy =
      CreateAppCookieStickinessPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateAppCookieStickinessPolicyResult"
      ( \s h x ->
          CreateAppCookieStickinessPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateAppCookieStickinessPolicy

instance
  Prelude.NFData
    CreateAppCookieStickinessPolicy

instance
  Prelude.ToHeaders
    CreateAppCookieStickinessPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    CreateAppCookieStickinessPolicy
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateAppCookieStickinessPolicy
  where
  toQuery CreateAppCookieStickinessPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CreateAppCookieStickinessPolicy" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "PolicyName" Prelude.=: policyName,
        "CookieName" Prelude.=: cookieName
      ]

-- | Contains the output for CreateAppCookieStickinessPolicy.
--
-- /See:/ 'newCreateAppCookieStickinessPolicyResponse' smart constructor.
data CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAppCookieStickinessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAppCookieStickinessPolicyResponse_httpStatus' - The response's http status code.
newCreateAppCookieStickinessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAppCookieStickinessPolicyResponse
newCreateAppCookieStickinessPolicyResponse
  pHttpStatus_ =
    CreateAppCookieStickinessPolicyResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createAppCookieStickinessPolicyResponse_httpStatus :: Lens.Lens' CreateAppCookieStickinessPolicyResponse Prelude.Int
createAppCookieStickinessPolicyResponse_httpStatus = Lens.lens (\CreateAppCookieStickinessPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateAppCookieStickinessPolicyResponse' {} a -> s {httpStatus = a} :: CreateAppCookieStickinessPolicyResponse)

instance
  Prelude.NFData
    CreateAppCookieStickinessPolicyResponse
