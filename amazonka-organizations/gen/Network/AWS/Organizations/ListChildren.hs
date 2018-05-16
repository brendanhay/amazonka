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
-- Module      : Network.AWS.Organizations.ListChildren
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the OUs or accounts that are contained in the specified parent OU or root. This operation, along with 'ListParents' enables you to traverse the tree structure that makes up this root.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListChildren
    (
    -- * Creating a Request
      listChildren
    , ListChildren
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults
    , lcParentId
    , lcChildType

    -- * Destructuring the Response
    , listChildrenResponse
    , ListChildrenResponse
    -- * Response Lenses
    , lcrsChildren
    , lcrsNextToken
    , lcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listChildren' smart constructor.
data ListChildren = ListChildren'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  , _lcParentId   :: !Text
  , _lcChildType  :: !ChildType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChildren' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lcMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'lcParentId' - The unique identifier (ID) for the parent root or OU whose children you want to list. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'lcChildType' - Filters the output to include only the specified child type.
listChildren
    :: Text -- ^ 'lcParentId'
    -> ChildType -- ^ 'lcChildType'
    -> ListChildren
listChildren pParentId_ pChildType_ =
  ListChildren'
    { _lcNextToken = Nothing
    , _lcMaxResults = Nothing
    , _lcParentId = pParentId_
    , _lcChildType = pChildType_
    }


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lcNextToken :: Lens' ListChildren (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lcMaxResults :: Lens' ListChildren (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

-- | The unique identifier (ID) for the parent root or OU whose children you want to list. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
lcParentId :: Lens' ListChildren Text
lcParentId = lens _lcParentId (\ s a -> s{_lcParentId = a})

-- | Filters the output to include only the specified child type.
lcChildType :: Lens' ListChildren ChildType
lcChildType = lens _lcChildType (\ s a -> s{_lcChildType = a})

instance AWSPager ListChildren where
        page rq rs
          | stop (rs ^. lcrsNextToken) = Nothing
          | stop (rs ^. lcrsChildren) = Nothing
          | otherwise =
            Just $ rq & lcNextToken .~ rs ^. lcrsNextToken

instance AWSRequest ListChildren where
        type Rs ListChildren = ListChildrenResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListChildrenResponse' <$>
                   (x .?> "Children" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListChildren where

instance NFData ListChildren where

instance ToHeaders ListChildren where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListChildren" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListChildren where
        toJSON ListChildren'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcNextToken,
                  ("MaxResults" .=) <$> _lcMaxResults,
                  Just ("ParentId" .= _lcParentId),
                  Just ("ChildType" .= _lcChildType)])

instance ToPath ListChildren where
        toPath = const "/"

instance ToQuery ListChildren where
        toQuery = const mempty

-- | /See:/ 'listChildrenResponse' smart constructor.
data ListChildrenResponse = ListChildrenResponse'
  { _lcrsChildren       :: !(Maybe [Child])
  , _lcrsNextToken      :: !(Maybe Text)
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChildrenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsChildren' - The list of children of the specified parent container.
--
-- * 'lcrsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listChildrenResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListChildrenResponse
listChildrenResponse pResponseStatus_ =
  ListChildrenResponse'
    { _lcrsChildren = Nothing
    , _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | The list of children of the specified parent container.
lcrsChildren :: Lens' ListChildrenResponse [Child]
lcrsChildren = lens _lcrsChildren (\ s a -> s{_lcrsChildren = a}) . _Default . _Coerce

-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lcrsNextToken :: Lens' ListChildrenResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListChildrenResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListChildrenResponse where
