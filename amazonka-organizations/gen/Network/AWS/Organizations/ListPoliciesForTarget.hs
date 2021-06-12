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
-- Module      : Network.AWS.Organizations.ListPoliciesForTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies that are directly attached to the specified target
-- root, organizational unit (OU), or account. You must specify the policy
-- type that you want included in the returned list.
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
module Network.AWS.Organizations.ListPoliciesForTarget
  ( -- * Creating a Request
    ListPoliciesForTarget (..),
    newListPoliciesForTarget,

    -- * Request Lenses
    listPoliciesForTarget_nextToken,
    listPoliciesForTarget_maxResults,
    listPoliciesForTarget_targetId,
    listPoliciesForTarget_filter,

    -- * Destructuring the Response
    ListPoliciesForTargetResponse (..),
    newListPoliciesForTargetResponse,

    -- * Response Lenses
    listPoliciesForTargetResponse_nextToken,
    listPoliciesForTargetResponse_policies,
    listPoliciesForTargetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPoliciesForTarget' smart constructor.
data ListPoliciesForTarget = ListPoliciesForTarget'
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
    -- | The unique identifier (ID) of the root, organizational unit, or account
    -- whose policies you want to list.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Account__ - A string that consists of exactly 12 digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    targetId :: Core.Text,
    -- | The type of policy that you want to include in the returned list. You
    -- must specify one of the following values:
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
-- Create a value of 'ListPoliciesForTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPoliciesForTarget_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listPoliciesForTarget_maxResults' - The total number of results that you want included on each page of the
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
-- 'targetId', 'listPoliciesForTarget_targetId' - The unique identifier (ID) of the root, organizational unit, or account
-- whose policies you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
--
-- 'filter'', 'listPoliciesForTarget_filter' - The type of policy that you want to include in the returned list. You
-- must specify one of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
newListPoliciesForTarget ::
  -- | 'targetId'
  Core.Text ->
  -- | 'filter''
  PolicyType ->
  ListPoliciesForTarget
newListPoliciesForTarget pTargetId_ pFilter_ =
  ListPoliciesForTarget'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      targetId = pTargetId_,
      filter' = pFilter_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listPoliciesForTarget_nextToken :: Lens.Lens' ListPoliciesForTarget (Core.Maybe Core.Text)
listPoliciesForTarget_nextToken = Lens.lens (\ListPoliciesForTarget' {nextToken} -> nextToken) (\s@ListPoliciesForTarget' {} a -> s {nextToken = a} :: ListPoliciesForTarget)

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
listPoliciesForTarget_maxResults :: Lens.Lens' ListPoliciesForTarget (Core.Maybe Core.Natural)
listPoliciesForTarget_maxResults = Lens.lens (\ListPoliciesForTarget' {maxResults} -> maxResults) (\s@ListPoliciesForTarget' {} a -> s {maxResults = a} :: ListPoliciesForTarget)

-- | The unique identifier (ID) of the root, organizational unit, or account
-- whose policies you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
listPoliciesForTarget_targetId :: Lens.Lens' ListPoliciesForTarget Core.Text
listPoliciesForTarget_targetId = Lens.lens (\ListPoliciesForTarget' {targetId} -> targetId) (\s@ListPoliciesForTarget' {} a -> s {targetId = a} :: ListPoliciesForTarget)

-- | The type of policy that you want to include in the returned list. You
-- must specify one of the following values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
listPoliciesForTarget_filter :: Lens.Lens' ListPoliciesForTarget PolicyType
listPoliciesForTarget_filter = Lens.lens (\ListPoliciesForTarget' {filter'} -> filter') (\s@ListPoliciesForTarget' {} a -> s {filter' = a} :: ListPoliciesForTarget)

instance Core.AWSPager ListPoliciesForTarget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoliciesForTargetResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoliciesForTargetResponse_policies
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPoliciesForTarget_nextToken
          Lens..~ rs
          Lens.^? listPoliciesForTargetResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListPoliciesForTarget where
  type
    AWSResponse ListPoliciesForTarget =
      ListPoliciesForTargetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesForTargetResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Policies" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPoliciesForTarget

instance Core.NFData ListPoliciesForTarget

instance Core.ToHeaders ListPoliciesForTarget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListPoliciesForTarget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPoliciesForTarget where
  toJSON ListPoliciesForTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("TargetId" Core..= targetId),
            Core.Just ("Filter" Core..= filter')
          ]
      )

instance Core.ToPath ListPoliciesForTarget where
  toPath = Core.const "/"

instance Core.ToQuery ListPoliciesForTarget where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPoliciesForTargetResponse' smart constructor.
data ListPoliciesForTargetResponse = ListPoliciesForTargetResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of policies that match the criteria in the request.
    policies :: Core.Maybe [PolicySummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPoliciesForTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPoliciesForTargetResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'policies', 'listPoliciesForTargetResponse_policies' - The list of policies that match the criteria in the request.
--
-- 'httpStatus', 'listPoliciesForTargetResponse_httpStatus' - The response's http status code.
newListPoliciesForTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPoliciesForTargetResponse
newListPoliciesForTargetResponse pHttpStatus_ =
  ListPoliciesForTargetResponse'
    { nextToken =
        Core.Nothing,
      policies = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listPoliciesForTargetResponse_nextToken :: Lens.Lens' ListPoliciesForTargetResponse (Core.Maybe Core.Text)
listPoliciesForTargetResponse_nextToken = Lens.lens (\ListPoliciesForTargetResponse' {nextToken} -> nextToken) (\s@ListPoliciesForTargetResponse' {} a -> s {nextToken = a} :: ListPoliciesForTargetResponse)

-- | The list of policies that match the criteria in the request.
listPoliciesForTargetResponse_policies :: Lens.Lens' ListPoliciesForTargetResponse (Core.Maybe [PolicySummary])
listPoliciesForTargetResponse_policies = Lens.lens (\ListPoliciesForTargetResponse' {policies} -> policies) (\s@ListPoliciesForTargetResponse' {} a -> s {policies = a} :: ListPoliciesForTargetResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPoliciesForTargetResponse_httpStatus :: Lens.Lens' ListPoliciesForTargetResponse Core.Int
listPoliciesForTargetResponse_httpStatus = Lens.lens (\ListPoliciesForTargetResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesForTargetResponse' {} a -> s {httpStatus = a} :: ListPoliciesForTargetResponse)

instance Core.NFData ListPoliciesForTargetResponse
