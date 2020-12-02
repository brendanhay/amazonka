{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of all policies in an organization of a specified type.
--
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListPolicies
  ( -- * Creating a Request
    listPolicies,
    ListPolicies,

    -- * Request Lenses
    lpNextToken,
    lpMaxResults,
    lpFilter,

    -- * Destructuring the Response
    listPoliciesResponse,
    ListPoliciesResponse,

    -- * Response Lenses
    lprsNextToken,
    lprsPolicies,
    lprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { _lpNextToken :: !(Maybe Text),
    _lpMaxResults :: !(Maybe Nat),
    _lpFilter :: !PolicyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lpMaxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'lpFilter' - Specifies the type of policy that you want to include in the response. You must specify one of the following values:     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
listPolicies ::
  -- | 'lpFilter'
  PolicyType ->
  ListPolicies
listPolicies pFilter_ =
  ListPolicies'
    { _lpNextToken = Nothing,
      _lpMaxResults = Nothing,
      _lpFilter = pFilter_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lpNextToken :: Lens' ListPolicies (Maybe Text)
lpNextToken = lens _lpNextToken (\s a -> s {_lpNextToken = a})

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lpMaxResults :: Lens' ListPolicies (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\s a -> s {_lpMaxResults = a}) . mapping _Nat

-- | Specifies the type of policy that you want to include in the response. You must specify one of the following values:     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>      * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
lpFilter :: Lens' ListPolicies PolicyType
lpFilter = lens _lpFilter (\s a -> s {_lpFilter = a})

instance AWSPager ListPolicies where
  page rq rs
    | stop (rs ^. lprsNextToken) = Nothing
    | stop (rs ^. lprsPolicies) = Nothing
    | otherwise = Just $ rq & lpNextToken .~ rs ^. lprsNextToken

instance AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Policies" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListPolicies

instance NFData ListPolicies

instance ToHeaders ListPolicies where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSOrganizationsV20161128.ListPolicies" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListPolicies where
  toJSON ListPolicies' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lpNextToken,
            ("MaxResults" .=) <$> _lpMaxResults,
            Just ("Filter" .= _lpFilter)
          ]
      )

instance ToPath ListPolicies where
  toPath = const "/"

instance ToQuery ListPolicies where
  toQuery = const mempty

-- | /See:/ 'listPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { _lprsNextToken ::
      !(Maybe Text),
    _lprsPolicies :: !(Maybe [PolicySummary]),
    _lprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lprsPolicies' - A list of policies that match the filter criteria in the request. The output list doesn't include the policy contents. To see the content for a policy, see 'DescribePolicy' .
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPoliciesResponse ::
  -- | 'lprsResponseStatus'
  Int ->
  ListPoliciesResponse
listPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { _lprsNextToken = Nothing,
      _lprsPolicies = Nothing,
      _lprsResponseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lprsNextToken :: Lens' ListPoliciesResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\s a -> s {_lprsNextToken = a})

-- | A list of policies that match the filter criteria in the request. The output list doesn't include the policy contents. To see the content for a policy, see 'DescribePolicy' .
lprsPolicies :: Lens' ListPoliciesResponse [PolicySummary]
lprsPolicies = lens _lprsPolicies (\s a -> s {_lprsPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPoliciesResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\s a -> s {_lprsResponseStatus = a})

instance NFData ListPoliciesResponse
