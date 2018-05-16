{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListPoliciesForTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies that are directly attached to the specified target root, organizational unit (OU), or account. You must specify the policy type that you want included in the returned list.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListPoliciesForTarget
    (
    -- * Creating a Request
      listPoliciesForTarget
    , ListPoliciesForTarget
    -- * Request Lenses
    , lpftNextToken
    , lpftMaxResults
    , lpftTargetId
    , lpftFilter

    -- * Destructuring the Response
    , listPoliciesForTargetResponse
    , ListPoliciesForTargetResponse
    -- * Response Lenses
    , lpftrsNextToken
    , lpftrsPolicies
    , lpftrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPoliciesForTarget' smart constructor.
data ListPoliciesForTarget = ListPoliciesForTarget'
  { _lpftNextToken  :: !(Maybe Text)
  , _lpftMaxResults :: !(Maybe Nat)
  , _lpftTargetId   :: !Text
  , _lpftFilter     :: !PolicyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesForTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpftNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lpftMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'lpftTargetId' - The unique identifier (ID) of the root, organizational unit, or account whose policies you want to list. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'lpftFilter' - The type of policy that you want to include in the returned list.
listPoliciesForTarget
    :: Text -- ^ 'lpftTargetId'
    -> PolicyType -- ^ 'lpftFilter'
    -> ListPoliciesForTarget
listPoliciesForTarget pTargetId_ pFilter_ =
  ListPoliciesForTarget'
    { _lpftNextToken = Nothing
    , _lpftMaxResults = Nothing
    , _lpftTargetId = pTargetId_
    , _lpftFilter = pFilter_
    }


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lpftNextToken :: Lens' ListPoliciesForTarget (Maybe Text)
lpftNextToken = lens _lpftNextToken (\ s a -> s{_lpftNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lpftMaxResults :: Lens' ListPoliciesForTarget (Maybe Natural)
lpftMaxResults = lens _lpftMaxResults (\ s a -> s{_lpftMaxResults = a}) . mapping _Nat

-- | The unique identifier (ID) of the root, organizational unit, or account whose policies you want to list. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
lpftTargetId :: Lens' ListPoliciesForTarget Text
lpftTargetId = lens _lpftTargetId (\ s a -> s{_lpftTargetId = a})

-- | The type of policy that you want to include in the returned list.
lpftFilter :: Lens' ListPoliciesForTarget PolicyType
lpftFilter = lens _lpftFilter (\ s a -> s{_lpftFilter = a})

instance AWSPager ListPoliciesForTarget where
        page rq rs
          | stop (rs ^. lpftrsNextToken) = Nothing
          | stop (rs ^. lpftrsPolicies) = Nothing
          | otherwise =
            Just $ rq & lpftNextToken .~ rs ^. lpftrsNextToken

instance AWSRequest ListPoliciesForTarget where
        type Rs ListPoliciesForTarget =
             ListPoliciesForTargetResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListPoliciesForTargetResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Policies" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPoliciesForTarget where

instance NFData ListPoliciesForTarget where

instance ToHeaders ListPoliciesForTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListPoliciesForTarget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPoliciesForTarget where
        toJSON ListPoliciesForTarget'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpftNextToken,
                  ("MaxResults" .=) <$> _lpftMaxResults,
                  Just ("TargetId" .= _lpftTargetId),
                  Just ("Filter" .= _lpftFilter)])

instance ToPath ListPoliciesForTarget where
        toPath = const "/"

instance ToQuery ListPoliciesForTarget where
        toQuery = const mempty

-- | /See:/ 'listPoliciesForTargetResponse' smart constructor.
data ListPoliciesForTargetResponse = ListPoliciesForTargetResponse'
  { _lpftrsNextToken      :: !(Maybe Text)
  , _lpftrsPolicies       :: !(Maybe [PolicySummary])
  , _lpftrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesForTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpftrsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lpftrsPolicies' - The list of policies that match the criteria in the request.
--
-- * 'lpftrsResponseStatus' - -- | The response status code.
listPoliciesForTargetResponse
    :: Int -- ^ 'lpftrsResponseStatus'
    -> ListPoliciesForTargetResponse
listPoliciesForTargetResponse pResponseStatus_ =
  ListPoliciesForTargetResponse'
    { _lpftrsNextToken = Nothing
    , _lpftrsPolicies = Nothing
    , _lpftrsResponseStatus = pResponseStatus_
    }


-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lpftrsNextToken :: Lens' ListPoliciesForTargetResponse (Maybe Text)
lpftrsNextToken = lens _lpftrsNextToken (\ s a -> s{_lpftrsNextToken = a})

-- | The list of policies that match the criteria in the request.
lpftrsPolicies :: Lens' ListPoliciesForTargetResponse [PolicySummary]
lpftrsPolicies = lens _lpftrsPolicies (\ s a -> s{_lpftrsPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
lpftrsResponseStatus :: Lens' ListPoliciesForTargetResponse Int
lpftrsResponseStatus = lens _lpftrsResponseStatus (\ s a -> s{_lpftrsResponseStatus = a})

instance NFData ListPoliciesForTargetResponse where
