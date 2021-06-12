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
-- Module      : Network.AWS.Organizations.ListPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of all policies in an organization of a specified
-- type.
--
-- Always check the @NextToken@ response parameter for a @null@ value when
-- calling a @List*@ operation. These operations can occasionally return an
-- empty set of results even when there are more results available. The
-- @NextToken@ response parameter value is @null@ /only/ when there are no
-- more results to display.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListPolicies
  ( -- * Creating a Request
    ListPolicies (..),
    newListPolicies,

    -- * Request Lenses
    listPolicies_nextToken,
    listPolicies_maxResults,
    listPolicies_filter,

    -- * Destructuring the Response
    ListPoliciesResponse (..),
    newListPoliciesResponse,

    -- * Response Lenses
    listPoliciesResponse_nextToken,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies the type of policy that you want to include in the response.
    -- You must specify one of the following values:
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    filter' :: PolicyType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicies_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listPolicies_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'filter'', 'listPolicies_filter' - Specifies the type of policy that you want to include in the response.
-- You must specify one of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
newListPolicies ::
  -- | 'filter''
  PolicyType ->
  ListPolicies
newListPolicies pFilter_ =
  ListPolicies'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = pFilter_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listPolicies_nextToken :: Lens.Lens' ListPolicies (Core.Maybe Core.Text)
listPolicies_nextToken = Lens.lens (\ListPolicies' {nextToken} -> nextToken) (\s@ListPolicies' {} a -> s {nextToken = a} :: ListPolicies)

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listPolicies_maxResults :: Lens.Lens' ListPolicies (Core.Maybe Core.Natural)
listPolicies_maxResults = Lens.lens (\ListPolicies' {maxResults} -> maxResults) (\s@ListPolicies' {} a -> s {maxResults = a} :: ListPolicies)

-- | Specifies the type of policy that you want to include in the response.
-- You must specify one of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
listPolicies_filter :: Lens.Lens' ListPolicies PolicyType
listPolicies_filter = Lens.lens (\ListPolicies' {filter'} -> filter') (\s@ListPolicies' {} a -> s {filter' = a} :: ListPolicies)

instance Core.AWSPager ListPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_policies Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPolicies_nextToken
          Lens..~ rs
          Lens.^? listPoliciesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListPolicies where
  type AWSResponse ListPolicies = ListPoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Policies" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPolicies

instance Core.NFData ListPolicies

instance Core.ToHeaders ListPolicies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListPolicies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPolicies where
  toJSON ListPolicies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("Filter" Core..= filter')
          ]
      )

instance Core.ToPath ListPolicies where
  toPath = Core.const "/"

instance Core.ToQuery ListPolicies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of policies that match the filter criteria in the request. The
    -- output list doesn\'t include the policy contents. To see the content for
    -- a policy, see DescribePolicy.
    policies :: Core.Maybe [PolicySummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPoliciesResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'policies', 'listPoliciesResponse_policies' - A list of policies that match the filter criteria in the request. The
-- output list doesn\'t include the policy contents. To see the content for
-- a policy, see DescribePolicy.
--
-- 'httpStatus', 'listPoliciesResponse_httpStatus' - The response's http status code.
newListPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPoliciesResponse
newListPoliciesResponse pHttpStatus_ =
  ListPoliciesResponse'
    { nextToken = Core.Nothing,
      policies = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listPoliciesResponse_nextToken :: Lens.Lens' ListPoliciesResponse (Core.Maybe Core.Text)
listPoliciesResponse_nextToken = Lens.lens (\ListPoliciesResponse' {nextToken} -> nextToken) (\s@ListPoliciesResponse' {} a -> s {nextToken = a} :: ListPoliciesResponse)

-- | A list of policies that match the filter criteria in the request. The
-- output list doesn\'t include the policy contents. To see the content for
-- a policy, see DescribePolicy.
listPoliciesResponse_policies :: Lens.Lens' ListPoliciesResponse (Core.Maybe [PolicySummary])
listPoliciesResponse_policies = Lens.lens (\ListPoliciesResponse' {policies} -> policies) (\s@ListPoliciesResponse' {} a -> s {policies = a} :: ListPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPoliciesResponse_httpStatus :: Lens.Lens' ListPoliciesResponse Core.Int
listPoliciesResponse_httpStatus = Lens.lens (\ListPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesResponse' {} a -> s {httpStatus = a} :: ListPoliciesResponse)

instance Core.NFData ListPoliciesResponse
