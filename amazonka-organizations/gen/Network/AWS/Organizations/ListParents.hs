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
-- Module      : Network.AWS.Organizations.ListParents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the root or organizational units (OUs) that serve as the immediate parent of the specified child OU or account. This operation, along with 'ListChildren' enables you to traverse the tree structure that makes up this root.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListParents
    (
    -- * Creating a Request
      listParents
    , ListParents
    -- * Request Lenses
    , lNextToken
    , lMaxResults
    , lChildId

    -- * Destructuring the Response
    , listParentsResponse
    , ListParentsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsParents
    , lrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listParents' smart constructor.
data ListParents = ListParents'
  { _lNextToken  :: !(Maybe Text)
  , _lMaxResults :: !(Maybe Nat)
  , _lChildId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListParents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'lChildId' - The unique identifier (ID) of the OU or account whose parent containers you want to list. Do not specify a root. The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
listParents
    :: Text -- ^ 'lChildId'
    -> ListParents
listParents pChildId_ =
  ListParents'
    {_lNextToken = Nothing, _lMaxResults = Nothing, _lChildId = pChildId_}


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lNextToken :: Lens' ListParents (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lMaxResults :: Lens' ListParents (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

-- | The unique identifier (ID) of the OU or account whose parent containers you want to list. Do not specify a root. The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:     * Account: a string that consists of exactly 12 digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
lChildId :: Lens' ListParents Text
lChildId = lens _lChildId (\ s a -> s{_lChildId = a})

instance AWSPager ListParents where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsParents) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListParents where
        type Rs ListParents = ListParentsResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListParentsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Parents" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListParents where

instance NFData ListParents where

instance ToHeaders ListParents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListParents" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListParents where
        toJSON ListParents'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lNextToken,
                  ("MaxResults" .=) <$> _lMaxResults,
                  Just ("ChildId" .= _lChildId)])

instance ToPath ListParents where
        toPath = const "/"

instance ToQuery ListParents where
        toQuery = const mempty

-- | /See:/ 'listParentsResponse' smart constructor.
data ListParentsResponse = ListParentsResponse'
  { _lrsNextToken      :: !(Maybe Text)
  , _lrsParents        :: !(Maybe [Parent])
  , _lrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListParentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lrsParents' - A list of parents for the specified child account or OU.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listParentsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListParentsResponse
listParentsResponse pResponseStatus_ =
  ListParentsResponse'
    { _lrsNextToken = Nothing
    , _lrsParents = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lrsNextToken :: Lens' ListParentsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | A list of parents for the specified child account or OU.
lrsParents :: Lens' ListParentsResponse [Parent]
lrsParents = lens _lrsParents (\ s a -> s{_lrsParents = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListParentsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListParentsResponse where
