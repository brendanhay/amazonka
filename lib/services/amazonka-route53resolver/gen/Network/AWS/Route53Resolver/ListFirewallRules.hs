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
-- Module      : Network.AWS.Route53Resolver.ListFirewallRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the firewall rules that you have defined for the specified
-- firewall rule group. DNS Firewall uses the rules in a rule group to
-- filter DNS network traffic for a VPC.
--
-- A single call might return only a partial list of the rules. For
-- information, see @MaxResults@.
--
-- This operation returns paginated results.
module Network.AWS.Route53Resolver.ListFirewallRules
  ( -- * Creating a Request
    ListFirewallRules (..),
    newListFirewallRules,

    -- * Request Lenses
    listFirewallRules_priority,
    listFirewallRules_action,
    listFirewallRules_nextToken,
    listFirewallRules_maxResults,
    listFirewallRules_firewallRuleGroupId,

    -- * Destructuring the Response
    ListFirewallRulesResponse (..),
    newListFirewallRulesResponse,

    -- * Response Lenses
    listFirewallRulesResponse_firewallRules,
    listFirewallRulesResponse_nextToken,
    listFirewallRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Resolver.Types

-- | /See:/ 'newListFirewallRules' smart constructor.
data ListFirewallRules = ListFirewallRules'
  { -- | Optional additional filter for the rules to retrieve.
    --
    -- The setting that determines the processing order of the rules in a rule
    -- group. DNS Firewall processes the rules in a rule group by order of
    -- priority, starting from the lowest setting.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Optional additional filter for the rules to retrieve.
    --
    -- The action that DNS Firewall should take on a DNS query when it matches
    -- one of the domains in the rule\'s domain list:
    --
    -- -   @ALLOW@ - Permit the request to go through.
    --
    -- -   @ALERT@ - Permit the request to go through but send an alert to the
    --     logs.
    --
    -- -   @BLOCK@ - Disallow the request. If this is specified, additional
    --     handling details are provided in the rule\'s @BlockResponse@
    --     setting.
    action :: Prelude.Maybe Action,
    -- | For the first call to this list request, omit this value.
    --
    -- When you request a list of objects, Resolver returns at most the number
    -- of objects specified in @MaxResults@. If more objects are available for
    -- retrieval, Resolver returns a @NextToken@ value in the response. To
    -- retrieve the next batch of objects, use the token that was returned for
    -- the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Resolver to return for this
    -- request. If more objects are available, in the response, Resolver
    -- provides a @NextToken@ value that you can use in a subsequent call to
    -- get the next batch of objects.
    --
    -- If you don\'t specify a value for @MaxResults@, Resolver returns up to
    -- 100 objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the firewall rule group that you want to
    -- retrieve the rules for.
    firewallRuleGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'listFirewallRules_priority' - Optional additional filter for the rules to retrieve.
--
-- The setting that determines the processing order of the rules in a rule
-- group. DNS Firewall processes the rules in a rule group by order of
-- priority, starting from the lowest setting.
--
-- 'action', 'listFirewallRules_action' - Optional additional filter for the rules to retrieve.
--
-- The action that DNS Firewall should take on a DNS query when it matches
-- one of the domains in the rule\'s domain list:
--
-- -   @ALLOW@ - Permit the request to go through.
--
-- -   @ALERT@ - Permit the request to go through but send an alert to the
--     logs.
--
-- -   @BLOCK@ - Disallow the request. If this is specified, additional
--     handling details are provided in the rule\'s @BlockResponse@
--     setting.
--
-- 'nextToken', 'listFirewallRules_nextToken' - For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
--
-- 'maxResults', 'listFirewallRules_maxResults' - The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
--
-- 'firewallRuleGroupId', 'listFirewallRules_firewallRuleGroupId' - The unique identifier of the firewall rule group that you want to
-- retrieve the rules for.
newListFirewallRules ::
  -- | 'firewallRuleGroupId'
  Prelude.Text ->
  ListFirewallRules
newListFirewallRules pFirewallRuleGroupId_ =
  ListFirewallRules'
    { priority = Prelude.Nothing,
      action = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      firewallRuleGroupId = pFirewallRuleGroupId_
    }

-- | Optional additional filter for the rules to retrieve.
--
-- The setting that determines the processing order of the rules in a rule
-- group. DNS Firewall processes the rules in a rule group by order of
-- priority, starting from the lowest setting.
listFirewallRules_priority :: Lens.Lens' ListFirewallRules (Prelude.Maybe Prelude.Int)
listFirewallRules_priority = Lens.lens (\ListFirewallRules' {priority} -> priority) (\s@ListFirewallRules' {} a -> s {priority = a} :: ListFirewallRules)

-- | Optional additional filter for the rules to retrieve.
--
-- The action that DNS Firewall should take on a DNS query when it matches
-- one of the domains in the rule\'s domain list:
--
-- -   @ALLOW@ - Permit the request to go through.
--
-- -   @ALERT@ - Permit the request to go through but send an alert to the
--     logs.
--
-- -   @BLOCK@ - Disallow the request. If this is specified, additional
--     handling details are provided in the rule\'s @BlockResponse@
--     setting.
listFirewallRules_action :: Lens.Lens' ListFirewallRules (Prelude.Maybe Action)
listFirewallRules_action = Lens.lens (\ListFirewallRules' {action} -> action) (\s@ListFirewallRules' {} a -> s {action = a} :: ListFirewallRules)

-- | For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
listFirewallRules_nextToken :: Lens.Lens' ListFirewallRules (Prelude.Maybe Prelude.Text)
listFirewallRules_nextToken = Lens.lens (\ListFirewallRules' {nextToken} -> nextToken) (\s@ListFirewallRules' {} a -> s {nextToken = a} :: ListFirewallRules)

-- | The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
listFirewallRules_maxResults :: Lens.Lens' ListFirewallRules (Prelude.Maybe Prelude.Natural)
listFirewallRules_maxResults = Lens.lens (\ListFirewallRules' {maxResults} -> maxResults) (\s@ListFirewallRules' {} a -> s {maxResults = a} :: ListFirewallRules)

-- | The unique identifier of the firewall rule group that you want to
-- retrieve the rules for.
listFirewallRules_firewallRuleGroupId :: Lens.Lens' ListFirewallRules Prelude.Text
listFirewallRules_firewallRuleGroupId = Lens.lens (\ListFirewallRules' {firewallRuleGroupId} -> firewallRuleGroupId) (\s@ListFirewallRules' {} a -> s {firewallRuleGroupId = a} :: ListFirewallRules)

instance Core.AWSPager ListFirewallRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallRulesResponse_firewallRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFirewallRules_nextToken
          Lens..~ rs
          Lens.^? listFirewallRulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFirewallRules where
  type
    AWSResponse ListFirewallRules =
      ListFirewallRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallRulesResponse'
            Prelude.<$> (x Core..?> "FirewallRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewallRules

instance Prelude.NFData ListFirewallRules

instance Core.ToHeaders ListFirewallRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.ListFirewallRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFirewallRules where
  toJSON ListFirewallRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Priority" Core..=) Prelude.<$> priority,
            ("Action" Core..=) Prelude.<$> action,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("FirewallRuleGroupId" Core..= firewallRuleGroupId)
          ]
      )

instance Core.ToPath ListFirewallRules where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFirewallRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallRulesResponse' smart constructor.
data ListFirewallRulesResponse = ListFirewallRulesResponse'
  { -- | A list of the rules that you have defined.
    --
    -- This might be a partial list of the firewall rules that you\'ve defined.
    -- For information, see @MaxResults@.
    firewallRules :: Prelude.Maybe [FirewallRule],
    -- | If objects are still available for retrieval, Resolver returns this
    -- token in the response. To retrieve the next batch of objects, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRules', 'listFirewallRulesResponse_firewallRules' - A list of the rules that you have defined.
--
-- This might be a partial list of the firewall rules that you\'ve defined.
-- For information, see @MaxResults@.
--
-- 'nextToken', 'listFirewallRulesResponse_nextToken' - If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
--
-- 'httpStatus', 'listFirewallRulesResponse_httpStatus' - The response's http status code.
newListFirewallRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallRulesResponse
newListFirewallRulesResponse pHttpStatus_ =
  ListFirewallRulesResponse'
    { firewallRules =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the rules that you have defined.
--
-- This might be a partial list of the firewall rules that you\'ve defined.
-- For information, see @MaxResults@.
listFirewallRulesResponse_firewallRules :: Lens.Lens' ListFirewallRulesResponse (Prelude.Maybe [FirewallRule])
listFirewallRulesResponse_firewallRules = Lens.lens (\ListFirewallRulesResponse' {firewallRules} -> firewallRules) (\s@ListFirewallRulesResponse' {} a -> s {firewallRules = a} :: ListFirewallRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
listFirewallRulesResponse_nextToken :: Lens.Lens' ListFirewallRulesResponse (Prelude.Maybe Prelude.Text)
listFirewallRulesResponse_nextToken = Lens.lens (\ListFirewallRulesResponse' {nextToken} -> nextToken) (\s@ListFirewallRulesResponse' {} a -> s {nextToken = a} :: ListFirewallRulesResponse)

-- | The response's http status code.
listFirewallRulesResponse_httpStatus :: Lens.Lens' ListFirewallRulesResponse Prelude.Int
listFirewallRulesResponse_httpStatus = Lens.lens (\ListFirewallRulesResponse' {httpStatus} -> httpStatus) (\s@ListFirewallRulesResponse' {} a -> s {httpStatus = a} :: ListFirewallRulesResponse)

instance Prelude.NFData ListFirewallRulesResponse
