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
-- Module      : Amazonka.NetworkFirewall.ListRuleGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for the rule groups that you have defined.
-- Depending on your setting for max results and the number of rule groups,
-- a single call might not return the full list.
--
-- This operation returns paginated results.
module Amazonka.NetworkFirewall.ListRuleGroups
  ( -- * Creating a Request
    ListRuleGroups (..),
    newListRuleGroups,

    -- * Request Lenses
    listRuleGroups_nextToken,
    listRuleGroups_maxResults,

    -- * Destructuring the Response
    ListRuleGroupsResponse (..),
    newListRuleGroupsResponse,

    -- * Response Lenses
    listRuleGroupsResponse_nextToken,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRuleGroups' smart constructor.
data ListRuleGroups = ListRuleGroups'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Network Firewall to return
    -- for this request. If more objects are available, in the response,
    -- Network Firewall provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuleGroups_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'maxResults', 'listRuleGroups_maxResults' - The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
newListRuleGroups ::
  ListRuleGroups
newListRuleGroups =
  ListRuleGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listRuleGroups_nextToken :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Text)
listRuleGroups_nextToken = Lens.lens (\ListRuleGroups' {nextToken} -> nextToken) (\s@ListRuleGroups' {} a -> s {nextToken = a} :: ListRuleGroups)

-- | The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listRuleGroups_maxResults :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Natural)
listRuleGroups_maxResults = Lens.lens (\ListRuleGroups' {maxResults} -> maxResults) (\s@ListRuleGroups' {} a -> s {maxResults = a} :: ListRuleGroups)

instance Core.AWSPager ListRuleGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRuleGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRuleGroupsResponse_ruleGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRuleGroups_nextToken
          Lens..~ rs
          Lens.^? listRuleGroupsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRuleGroups where
  type
    AWSResponse ListRuleGroups =
      ListRuleGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "RuleGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuleGroups

instance Prelude.NFData ListRuleGroups

instance Core.ToHeaders ListRuleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.ListRuleGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRuleGroups where
  toJSON ListRuleGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListRuleGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRuleGroupsResponse' smart constructor.
data ListRuleGroupsResponse = ListRuleGroupsResponse'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The rule group metadata objects that you\'ve defined. Depending on your
    -- setting for max results and the number of rule groups, this might not be
    -- the full list.
    ruleGroups :: Prelude.Maybe [RuleGroupMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuleGroupsResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'ruleGroups', 'listRuleGroupsResponse_ruleGroups' - The rule group metadata objects that you\'ve defined. Depending on your
-- setting for max results and the number of rule groups, this might not be
-- the full list.
--
-- 'httpStatus', 'listRuleGroupsResponse_httpStatus' - The response's http status code.
newListRuleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRuleGroupsResponse
newListRuleGroupsResponse pHttpStatus_ =
  ListRuleGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      ruleGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listRuleGroupsResponse_nextToken :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listRuleGroupsResponse_nextToken = Lens.lens (\ListRuleGroupsResponse' {nextToken} -> nextToken) (\s@ListRuleGroupsResponse' {} a -> s {nextToken = a} :: ListRuleGroupsResponse)

-- | The rule group metadata objects that you\'ve defined. Depending on your
-- setting for max results and the number of rule groups, this might not be
-- the full list.
listRuleGroupsResponse_ruleGroups :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe [RuleGroupMetadata])
listRuleGroupsResponse_ruleGroups = Lens.lens (\ListRuleGroupsResponse' {ruleGroups} -> ruleGroups) (\s@ListRuleGroupsResponse' {} a -> s {ruleGroups = a} :: ListRuleGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRuleGroupsResponse_httpStatus :: Lens.Lens' ListRuleGroupsResponse Prelude.Int
listRuleGroupsResponse_httpStatus = Lens.lens (\ListRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListRuleGroupsResponse)

instance Prelude.NFData ListRuleGroupsResponse
