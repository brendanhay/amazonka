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
-- Module      : Amazonka.WAFV2.CheckCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the web ACL capacity unit (WCU) requirements for a specified
-- scope and set of rules. You can use this to check the capacity
-- requirements for the rules you want to use in a RuleGroup or WebACL.
--
-- WAF uses WCUs to calculate and control the operating resources that are
-- used to run your rules, rule groups, and web ACLs. WAF calculates
-- capacity differently for each rule type, to reflect the relative cost of
-- each rule. Simple rules that cost little to run use fewer WCUs than more
-- complex rules that use more processing power. Rule group capacity is
-- fixed at creation, which helps users plan their web ACL WCU usage when
-- they use a rule group. The WCU limit for web ACLs is 1,500.
module Amazonka.WAFV2.CheckCapacity
  ( -- * Creating a Request
    CheckCapacity (..),
    newCheckCapacity,

    -- * Request Lenses
    checkCapacity_scope,
    checkCapacity_rules,

    -- * Destructuring the Response
    CheckCapacityResponse (..),
    newCheckCapacityResponse,

    -- * Response Lenses
    checkCapacityResponse_capacity,
    checkCapacityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newCheckCapacity' smart constructor.
data CheckCapacity = CheckCapacity'
  { -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | An array of Rule that you\'re configuring to use in a rule group or web
    -- ACL.
    rules :: [Rule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'checkCapacity_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'rules', 'checkCapacity_rules' - An array of Rule that you\'re configuring to use in a rule group or web
-- ACL.
newCheckCapacity ::
  -- | 'scope'
  Scope ->
  CheckCapacity
newCheckCapacity pScope_ =
  CheckCapacity'
    { scope = pScope_,
      rules = Prelude.mempty
    }

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
checkCapacity_scope :: Lens.Lens' CheckCapacity Scope
checkCapacity_scope = Lens.lens (\CheckCapacity' {scope} -> scope) (\s@CheckCapacity' {} a -> s {scope = a} :: CheckCapacity)

-- | An array of Rule that you\'re configuring to use in a rule group or web
-- ACL.
checkCapacity_rules :: Lens.Lens' CheckCapacity [Rule]
checkCapacity_rules = Lens.lens (\CheckCapacity' {rules} -> rules) (\s@CheckCapacity' {} a -> s {rules = a} :: CheckCapacity) Prelude.. Lens.coerced

instance Core.AWSRequest CheckCapacity where
  type
    AWSResponse CheckCapacity =
      CheckCapacityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckCapacityResponse'
            Prelude.<$> (x Data..?> "Capacity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckCapacity where
  hashWithSalt _salt CheckCapacity' {..} =
    _salt `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CheckCapacity where
  rnf CheckCapacity' {..} =
    Prelude.rnf scope `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders CheckCapacity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.CheckCapacity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CheckCapacity where
  toJSON CheckCapacity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath CheckCapacity where
  toPath = Prelude.const "/"

instance Data.ToQuery CheckCapacity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCheckCapacityResponse' smart constructor.
data CheckCapacityResponse = CheckCapacityResponse'
  { -- | The capacity required by the rules and scope.
    capacity :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckCapacityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacity', 'checkCapacityResponse_capacity' - The capacity required by the rules and scope.
--
-- 'httpStatus', 'checkCapacityResponse_httpStatus' - The response's http status code.
newCheckCapacityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckCapacityResponse
newCheckCapacityResponse pHttpStatus_ =
  CheckCapacityResponse'
    { capacity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capacity required by the rules and scope.
checkCapacityResponse_capacity :: Lens.Lens' CheckCapacityResponse (Prelude.Maybe Prelude.Natural)
checkCapacityResponse_capacity = Lens.lens (\CheckCapacityResponse' {capacity} -> capacity) (\s@CheckCapacityResponse' {} a -> s {capacity = a} :: CheckCapacityResponse)

-- | The response's http status code.
checkCapacityResponse_httpStatus :: Lens.Lens' CheckCapacityResponse Prelude.Int
checkCapacityResponse_httpStatus = Lens.lens (\CheckCapacityResponse' {httpStatus} -> httpStatus) (\s@CheckCapacityResponse' {} a -> s {httpStatus = a} :: CheckCapacityResponse)

instance Prelude.NFData CheckCapacityResponse where
  rnf CheckCapacityResponse' {..} =
    Prelude.rnf capacity
      `Prelude.seq` Prelude.rnf httpStatus
