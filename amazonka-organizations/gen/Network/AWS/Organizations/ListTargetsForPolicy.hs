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
-- Module      : Network.AWS.Organizations.ListTargetsForPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the roots, OUs, and accounts to which the specified policy is attached.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTargetsForPolicy
    (
    -- * Creating a Request
      listTargetsForPolicy
    , ListTargetsForPolicy
    -- * Request Lenses
    , ltfpNextToken
    , ltfpMaxResults
    , ltfpPolicyId

    -- * Destructuring the Response
    , listTargetsForPolicyResponse
    , ListTargetsForPolicyResponse
    -- * Response Lenses
    , ltfprsNextToken
    , ltfprsTargets
    , ltfprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { _ltfpNextToken  :: !(Maybe Text)
  , _ltfpMaxResults :: !(Maybe Nat)
  , _ltfpPolicyId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTargetsForPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfpNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'ltfpMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'ltfpPolicyId' - The unique identifier (ID) of the policy for which you want to know its attachments. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
listTargetsForPolicy
    :: Text -- ^ 'ltfpPolicyId'
    -> ListTargetsForPolicy
listTargetsForPolicy pPolicyId_ =
  ListTargetsForPolicy'
    { _ltfpNextToken = Nothing
    , _ltfpMaxResults = Nothing
    , _ltfpPolicyId = pPolicyId_
    }


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
ltfpNextToken :: Lens' ListTargetsForPolicy (Maybe Text)
ltfpNextToken = lens _ltfpNextToken (\ s a -> s{_ltfpNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
ltfpMaxResults :: Lens' ListTargetsForPolicy (Maybe Natural)
ltfpMaxResults = lens _ltfpMaxResults (\ s a -> s{_ltfpMaxResults = a}) . mapping _Nat

-- | The unique identifier (ID) of the policy for which you want to know its attachments. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
ltfpPolicyId :: Lens' ListTargetsForPolicy Text
ltfpPolicyId = lens _ltfpPolicyId (\ s a -> s{_ltfpPolicyId = a})

instance AWSPager ListTargetsForPolicy where
        page rq rs
          | stop (rs ^. ltfprsNextToken) = Nothing
          | stop (rs ^. ltfprsTargets) = Nothing
          | otherwise =
            Just $ rq & ltfpNextToken .~ rs ^. ltfprsNextToken

instance AWSRequest ListTargetsForPolicy where
        type Rs ListTargetsForPolicy =
             ListTargetsForPolicyResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListTargetsForPolicyResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Targets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTargetsForPolicy where

instance NFData ListTargetsForPolicy where

instance ToHeaders ListTargetsForPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListTargetsForPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTargetsForPolicy where
        toJSON ListTargetsForPolicy'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltfpNextToken,
                  ("MaxResults" .=) <$> _ltfpMaxResults,
                  Just ("PolicyId" .= _ltfpPolicyId)])

instance ToPath ListTargetsForPolicy where
        toPath = const "/"

instance ToQuery ListTargetsForPolicy where
        toQuery = const mempty

-- | /See:/ 'listTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { _ltfprsNextToken      :: !(Maybe Text)
  , _ltfprsTargets        :: !(Maybe [PolicyTargetSummary])
  , _ltfprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTargetsForPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfprsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'ltfprsTargets' - A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
--
-- * 'ltfprsResponseStatus' - -- | The response status code.
listTargetsForPolicyResponse
    :: Int -- ^ 'ltfprsResponseStatus'
    -> ListTargetsForPolicyResponse
listTargetsForPolicyResponse pResponseStatus_ =
  ListTargetsForPolicyResponse'
    { _ltfprsNextToken = Nothing
    , _ltfprsTargets = Nothing
    , _ltfprsResponseStatus = pResponseStatus_
    }


-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
ltfprsNextToken :: Lens' ListTargetsForPolicyResponse (Maybe Text)
ltfprsNextToken = lens _ltfprsNextToken (\ s a -> s{_ltfprsNextToken = a})

-- | A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
ltfprsTargets :: Lens' ListTargetsForPolicyResponse [PolicyTargetSummary]
ltfprsTargets = lens _ltfprsTargets (\ s a -> s{_ltfprsTargets = a}) . _Default . _Coerce

-- | -- | The response status code.
ltfprsResponseStatus :: Lens' ListTargetsForPolicyResponse Int
ltfprsResponseStatus = lens _ltfprsResponseStatus (\ s a -> s{_ltfprsResponseStatus = a})

instance NFData ListTargetsForPolicyResponse where
