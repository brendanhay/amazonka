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
-- Module      : Amazonka.Organizations.ListPoliciesForTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListPoliciesForTarget
  ( -- * Creating a Request
    ListPoliciesForTarget (..),
    newListPoliciesForTarget,

    -- * Request Lenses
    listPoliciesForTarget_maxResults,
    listPoliciesForTarget_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPoliciesForTarget' smart constructor.
data ListPoliciesForTarget = ListPoliciesForTarget'
  { -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    targetId :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPoliciesForTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'listPoliciesForTarget_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
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
  Prelude.Text ->
  -- | 'filter''
  PolicyType ->
  ListPoliciesForTarget
newListPoliciesForTarget pTargetId_ pFilter_ =
  ListPoliciesForTarget'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      targetId = pTargetId_,
      filter' = pFilter_
    }

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
listPoliciesForTarget_maxResults :: Lens.Lens' ListPoliciesForTarget (Prelude.Maybe Prelude.Natural)
listPoliciesForTarget_maxResults = Lens.lens (\ListPoliciesForTarget' {maxResults} -> maxResults) (\s@ListPoliciesForTarget' {} a -> s {maxResults = a} :: ListPoliciesForTarget)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listPoliciesForTarget_nextToken :: Lens.Lens' ListPoliciesForTarget (Prelude.Maybe Prelude.Text)
listPoliciesForTarget_nextToken = Lens.lens (\ListPoliciesForTarget' {nextToken} -> nextToken) (\s@ListPoliciesForTarget' {} a -> s {nextToken = a} :: ListPoliciesForTarget)

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
listPoliciesForTarget_targetId :: Lens.Lens' ListPoliciesForTarget Prelude.Text
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
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPoliciesForTargetResponse_policies
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPoliciesForTarget_nextToken
          Lens..~ rs
          Lens.^? listPoliciesForTargetResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPoliciesForTarget where
  type
    AWSResponse ListPoliciesForTarget =
      ListPoliciesForTargetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPoliciesForTargetResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Policies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPoliciesForTarget where
  hashWithSalt _salt ListPoliciesForTarget' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData ListPoliciesForTarget where
  rnf ListPoliciesForTarget' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf filter'

instance Data.ToHeaders ListPoliciesForTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListPoliciesForTarget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPoliciesForTarget where
  toJSON ListPoliciesForTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("TargetId" Data..= targetId),
            Prelude.Just ("Filter" Data..= filter')
          ]
      )

instance Data.ToPath ListPoliciesForTarget where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPoliciesForTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPoliciesForTargetResponse' smart constructor.
data ListPoliciesForTargetResponse = ListPoliciesForTargetResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of policies that match the criteria in the request.
    policies :: Prelude.Maybe [PolicySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListPoliciesForTargetResponse
newListPoliciesForTargetResponse pHttpStatus_ =
  ListPoliciesForTargetResponse'
    { nextToken =
        Prelude.Nothing,
      policies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listPoliciesForTargetResponse_nextToken :: Lens.Lens' ListPoliciesForTargetResponse (Prelude.Maybe Prelude.Text)
listPoliciesForTargetResponse_nextToken = Lens.lens (\ListPoliciesForTargetResponse' {nextToken} -> nextToken) (\s@ListPoliciesForTargetResponse' {} a -> s {nextToken = a} :: ListPoliciesForTargetResponse)

-- | The list of policies that match the criteria in the request.
listPoliciesForTargetResponse_policies :: Lens.Lens' ListPoliciesForTargetResponse (Prelude.Maybe [PolicySummary])
listPoliciesForTargetResponse_policies = Lens.lens (\ListPoliciesForTargetResponse' {policies} -> policies) (\s@ListPoliciesForTargetResponse' {} a -> s {policies = a} :: ListPoliciesForTargetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPoliciesForTargetResponse_httpStatus :: Lens.Lens' ListPoliciesForTargetResponse Prelude.Int
listPoliciesForTargetResponse_httpStatus = Lens.lens (\ListPoliciesForTargetResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesForTargetResponse' {} a -> s {httpStatus = a} :: ListPoliciesForTargetResponse)

instance Prelude.NFData ListPoliciesForTargetResponse where
  rnf ListPoliciesForTargetResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf httpStatus
