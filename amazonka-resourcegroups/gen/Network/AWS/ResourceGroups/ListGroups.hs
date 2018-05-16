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
-- Module      : Network.AWS.ResourceGroups.ListGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing resource groups in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroups
    (
    -- * Creating a Request
      listGroups
    , ListGroups
    -- * Request Lenses
    , lgNextToken
    , lgMaxResults

    -- * Destructuring the Response
    , listGroupsResponse
    , ListGroupsResponse
    -- * Response Lenses
    , lgrsGroups
    , lgrsNextToken
    , lgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'listGroups' smart constructor.
data ListGroups = ListGroups'
  { _lgNextToken  :: !(Maybe Text)
  , _lgMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgNextToken' - The NextToken value that is returned in a paginated @ListGroups@ request. To get the next page of results, run the call again, add the NextToken parameter, and specify the NextToken value.
--
-- * 'lgMaxResults' - The maximum number of resource group results that are returned by ListGroups in paginated output. By default, this number is 50.
listGroups
    :: ListGroups
listGroups = ListGroups' {_lgNextToken = Nothing, _lgMaxResults = Nothing}


-- | The NextToken value that is returned in a paginated @ListGroups@ request. To get the next page of results, run the call again, add the NextToken parameter, and specify the NextToken value.
lgNextToken :: Lens' ListGroups (Maybe Text)
lgNextToken = lens _lgNextToken (\ s a -> s{_lgNextToken = a})

-- | The maximum number of resource group results that are returned by ListGroups in paginated output. By default, this number is 50.
lgMaxResults :: Lens' ListGroups (Maybe Natural)
lgMaxResults = lens _lgMaxResults (\ s a -> s{_lgMaxResults = a}) . mapping _Nat

instance AWSPager ListGroups where
        page rq rs
          | stop (rs ^. lgrsNextToken) = Nothing
          | stop (rs ^. lgrsGroups) = Nothing
          | otherwise =
            Just $ rq & lgNextToken .~ rs ^. lgrsNextToken

instance AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        request = get resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupsResponse' <$>
                   (x .?> "Groups" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGroups where

instance NFData ListGroups where

instance ToHeaders ListGroups where
        toHeaders = const mempty

instance ToPath ListGroups where
        toPath = const "/groups"

instance ToQuery ListGroups where
        toQuery ListGroups'{..}
          = mconcat
              ["nextToken" =: _lgNextToken,
               "maxResults" =: _lgMaxResults]

-- | /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { _lgrsGroups         :: !(Maybe [Group])
  , _lgrsNextToken      :: !(Maybe Text)
  , _lgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsGroups' - A list of resource groups.
--
-- * 'lgrsNextToken' - The NextToken value to include in a subsequent @ListGroups@ request, to get more results.
--
-- * 'lgrsResponseStatus' - -- | The response status code.
listGroupsResponse
    :: Int -- ^ 'lgrsResponseStatus'
    -> ListGroupsResponse
listGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { _lgrsGroups = Nothing
    , _lgrsNextToken = Nothing
    , _lgrsResponseStatus = pResponseStatus_
    }


-- | A list of resource groups.
lgrsGroups :: Lens' ListGroupsResponse [Group]
lgrsGroups = lens _lgrsGroups (\ s a -> s{_lgrsGroups = a}) . _Default . _Coerce

-- | The NextToken value to include in a subsequent @ListGroups@ request, to get more results.
lgrsNextToken :: Lens' ListGroupsResponse (Maybe Text)
lgrsNextToken = lens _lgrsNextToken (\ s a -> s{_lgrsNextToken = a})

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGroupsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a})

instance NFData ListGroupsResponse where
