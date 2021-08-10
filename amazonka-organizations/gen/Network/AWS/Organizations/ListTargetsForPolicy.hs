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
-- Module      : Network.AWS.Organizations.ListTargetsForPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the roots, organizational units (OUs), and accounts that the
-- specified policy is attached to.
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
module Network.AWS.Organizations.ListTargetsForPolicy
  ( -- * Creating a Request
    ListTargetsForPolicy (..),
    newListTargetsForPolicy,

    -- * Request Lenses
    listTargetsForPolicy_nextToken,
    listTargetsForPolicy_maxResults,
    listTargetsForPolicy_policyId,

    -- * Destructuring the Response
    ListTargetsForPolicyResponse (..),
    newListTargetsForPolicyResponse,

    -- * Response Lenses
    listTargetsForPolicyResponse_nextToken,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier (ID) of the policy whose attachments you want to
    -- know.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsForPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsForPolicy_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listTargetsForPolicy_maxResults' - The total number of results that you want included on each page of the
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
-- 'policyId', 'listTargetsForPolicy_policyId' - The unique identifier (ID) of the policy whose attachments you want to
-- know.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
newListTargetsForPolicy ::
  -- | 'policyId'
  Prelude.Text ->
  ListTargetsForPolicy
newListTargetsForPolicy pPolicyId_ =
  ListTargetsForPolicy'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listTargetsForPolicy_nextToken :: Lens.Lens' ListTargetsForPolicy (Prelude.Maybe Prelude.Text)
listTargetsForPolicy_nextToken = Lens.lens (\ListTargetsForPolicy' {nextToken} -> nextToken) (\s@ListTargetsForPolicy' {} a -> s {nextToken = a} :: ListTargetsForPolicy)

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
listTargetsForPolicy_maxResults :: Lens.Lens' ListTargetsForPolicy (Prelude.Maybe Prelude.Natural)
listTargetsForPolicy_maxResults = Lens.lens (\ListTargetsForPolicy' {maxResults} -> maxResults) (\s@ListTargetsForPolicy' {} a -> s {maxResults = a} :: ListTargetsForPolicy)

-- | The unique identifier (ID) of the policy whose attachments you want to
-- know.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
listTargetsForPolicy_policyId :: Lens.Lens' ListTargetsForPolicy Prelude.Text
listTargetsForPolicy_policyId = Lens.lens (\ListTargetsForPolicy' {policyId} -> policyId) (\s@ListTargetsForPolicy' {} a -> s {policyId = a} :: ListTargetsForPolicy)

instance Core.AWSPager ListTargetsForPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsForPolicyResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsForPolicyResponse_targets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTargetsForPolicy_nextToken
          Lens..~ rs
          Lens.^? listTargetsForPolicyResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTargetsForPolicy where
  type
    AWSResponse ListTargetsForPolicy =
      ListTargetsForPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTargetsForPolicy

instance Prelude.NFData ListTargetsForPolicy

instance Core.ToHeaders ListTargetsForPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListTargetsForPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTargetsForPolicy where
  toJSON ListTargetsForPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("PolicyId" Core..= policyId)
          ]
      )

instance Core.ToPath ListTargetsForPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTargetsForPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of structures, each of which contains details about one of the
    -- entities to which the specified policy is attached.
    targets :: Prelude.Maybe [PolicyTargetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsForPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsForPolicyResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'targets', 'listTargetsForPolicyResponse_targets' - A list of structures, each of which contains details about one of the
-- entities to which the specified policy is attached.
--
-- 'httpStatus', 'listTargetsForPolicyResponse_httpStatus' - The response's http status code.
newListTargetsForPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetsForPolicyResponse
newListTargetsForPolicyResponse pHttpStatus_ =
  ListTargetsForPolicyResponse'
    { nextToken =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listTargetsForPolicyResponse_nextToken :: Lens.Lens' ListTargetsForPolicyResponse (Prelude.Maybe Prelude.Text)
listTargetsForPolicyResponse_nextToken = Lens.lens (\ListTargetsForPolicyResponse' {nextToken} -> nextToken) (\s@ListTargetsForPolicyResponse' {} a -> s {nextToken = a} :: ListTargetsForPolicyResponse)

-- | A list of structures, each of which contains details about one of the
-- entities to which the specified policy is attached.
listTargetsForPolicyResponse_targets :: Lens.Lens' ListTargetsForPolicyResponse (Prelude.Maybe [PolicyTargetSummary])
listTargetsForPolicyResponse_targets = Lens.lens (\ListTargetsForPolicyResponse' {targets} -> targets) (\s@ListTargetsForPolicyResponse' {} a -> s {targets = a} :: ListTargetsForPolicyResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTargetsForPolicyResponse_httpStatus :: Lens.Lens' ListTargetsForPolicyResponse Prelude.Int
listTargetsForPolicyResponse_httpStatus = Lens.lens (\ListTargetsForPolicyResponse' {httpStatus} -> httpStatus) (\s@ListTargetsForPolicyResponse' {} a -> s {httpStatus = a} :: ListTargetsForPolicyResponse)

instance Prelude.NFData ListTargetsForPolicyResponse
