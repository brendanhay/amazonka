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
-- Module      : Network.AWS.ELB.CreateLBCookieStickinessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a stickiness policy with sticky session lifetimes controlled
-- by the lifetime of the browser (user-agent) or a specified expiration
-- period. This policy can be associated only with HTTP\/HTTPS listeners.
--
-- When a load balancer implements this policy, the load balancer uses a
-- special cookie to track the instance for each request. When the load
-- balancer receives a request, it first checks to see if this cookie is
-- present in the request. If so, the load balancer sends the request to
-- the application server specified in the cookie. If not, the load
-- balancer sends the request to a server that is chosen based on the
-- existing load-balancing algorithm.
--
-- A cookie is inserted into the response for binding subsequent requests
-- from the same user to that server. The validity of the cookie is based
-- on the cookie expiration time, which is specified in the policy
-- configuration.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration Duration-Based Session Stickiness>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.CreateLBCookieStickinessPolicy
  ( -- * Creating a Request
    CreateLBCookieStickinessPolicy (..),
    newCreateLBCookieStickinessPolicy,

    -- * Request Lenses
    createLBCookieStickinessPolicy_cookieExpirationPeriod,
    createLBCookieStickinessPolicy_loadBalancerName,
    createLBCookieStickinessPolicy_policyName,

    -- * Destructuring the Response
    CreateLBCookieStickinessPolicyResponse (..),
    newCreateLBCookieStickinessPolicyResponse,

    -- * Response Lenses
    createLBCookieStickinessPolicyResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLBCookieStickinessPolicy.
--
-- /See:/ 'newCreateLBCookieStickinessPolicy' smart constructor.
data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy'
  { -- | The time period, in seconds, after which the cookie should be considered
    -- stale. If you do not specify this parameter, the default value is 0,
    -- which indicates that the sticky session should last for the duration of
    -- the browser session.
    cookieExpirationPeriod :: Prelude.Maybe Prelude.Integer,
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The name of the policy being created. Policy names must consist of
    -- alphanumeric characters and dashes (-). This name must be unique within
    -- the set of policies for this load balancer.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLBCookieStickinessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cookieExpirationPeriod', 'createLBCookieStickinessPolicy_cookieExpirationPeriod' - The time period, in seconds, after which the cookie should be considered
-- stale. If you do not specify this parameter, the default value is 0,
-- which indicates that the sticky session should last for the duration of
-- the browser session.
--
-- 'loadBalancerName', 'createLBCookieStickinessPolicy_loadBalancerName' - The name of the load balancer.
--
-- 'policyName', 'createLBCookieStickinessPolicy_policyName' - The name of the policy being created. Policy names must consist of
-- alphanumeric characters and dashes (-). This name must be unique within
-- the set of policies for this load balancer.
newCreateLBCookieStickinessPolicy ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  CreateLBCookieStickinessPolicy
newCreateLBCookieStickinessPolicy
  pLoadBalancerName_
  pPolicyName_ =
    CreateLBCookieStickinessPolicy'
      { cookieExpirationPeriod =
          Prelude.Nothing,
        loadBalancerName = pLoadBalancerName_,
        policyName = pPolicyName_
      }

-- | The time period, in seconds, after which the cookie should be considered
-- stale. If you do not specify this parameter, the default value is 0,
-- which indicates that the sticky session should last for the duration of
-- the browser session.
createLBCookieStickinessPolicy_cookieExpirationPeriod :: Lens.Lens' CreateLBCookieStickinessPolicy (Prelude.Maybe Prelude.Integer)
createLBCookieStickinessPolicy_cookieExpirationPeriod = Lens.lens (\CreateLBCookieStickinessPolicy' {cookieExpirationPeriod} -> cookieExpirationPeriod) (\s@CreateLBCookieStickinessPolicy' {} a -> s {cookieExpirationPeriod = a} :: CreateLBCookieStickinessPolicy)

-- | The name of the load balancer.
createLBCookieStickinessPolicy_loadBalancerName :: Lens.Lens' CreateLBCookieStickinessPolicy Prelude.Text
createLBCookieStickinessPolicy_loadBalancerName = Lens.lens (\CreateLBCookieStickinessPolicy' {loadBalancerName} -> loadBalancerName) (\s@CreateLBCookieStickinessPolicy' {} a -> s {loadBalancerName = a} :: CreateLBCookieStickinessPolicy)

-- | The name of the policy being created. Policy names must consist of
-- alphanumeric characters and dashes (-). This name must be unique within
-- the set of policies for this load balancer.
createLBCookieStickinessPolicy_policyName :: Lens.Lens' CreateLBCookieStickinessPolicy Prelude.Text
createLBCookieStickinessPolicy_policyName = Lens.lens (\CreateLBCookieStickinessPolicy' {policyName} -> policyName) (\s@CreateLBCookieStickinessPolicy' {} a -> s {policyName = a} :: CreateLBCookieStickinessPolicy)

instance
  Prelude.AWSRequest
    CreateLBCookieStickinessPolicy
  where
  type
    Rs CreateLBCookieStickinessPolicy =
      CreateLBCookieStickinessPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateLBCookieStickinessPolicyResult"
      ( \s h x ->
          CreateLBCookieStickinessPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLBCookieStickinessPolicy

instance
  Prelude.NFData
    CreateLBCookieStickinessPolicy

instance
  Prelude.ToHeaders
    CreateLBCookieStickinessPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    CreateLBCookieStickinessPolicy
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateLBCookieStickinessPolicy
  where
  toQuery CreateLBCookieStickinessPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CreateLBCookieStickinessPolicy" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "CookieExpirationPeriod"
          Prelude.=: cookieExpirationPeriod,
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "PolicyName" Prelude.=: policyName
      ]

-- | Contains the output for CreateLBCookieStickinessPolicy.
--
-- /See:/ 'newCreateLBCookieStickinessPolicyResponse' smart constructor.
data CreateLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLBCookieStickinessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLBCookieStickinessPolicyResponse_httpStatus' - The response's http status code.
newCreateLBCookieStickinessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLBCookieStickinessPolicyResponse
newCreateLBCookieStickinessPolicyResponse
  pHttpStatus_ =
    CreateLBCookieStickinessPolicyResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createLBCookieStickinessPolicyResponse_httpStatus :: Lens.Lens' CreateLBCookieStickinessPolicyResponse Prelude.Int
createLBCookieStickinessPolicyResponse_httpStatus = Lens.lens (\CreateLBCookieStickinessPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateLBCookieStickinessPolicyResponse' {} a -> s {httpStatus = a} :: CreateLBCookieStickinessPolicyResponse)

instance
  Prelude.NFData
    CreateLBCookieStickinessPolicyResponse
