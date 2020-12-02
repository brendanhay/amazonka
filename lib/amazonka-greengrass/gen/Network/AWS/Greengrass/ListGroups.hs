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
-- Module      : Network.AWS.Greengrass.ListGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of groups.
module Network.AWS.Greengrass.ListGroups
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

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGroups' smart constructor.
data ListGroups = ListGroups'
  { _lgNextToken  :: !(Maybe Text)
  , _lgMaxResults :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lgMaxResults' - The maximum number of results to be returned per request.
listGroups
    :: ListGroups
listGroups = ListGroups' {_lgNextToken = Nothing, _lgMaxResults = Nothing}


-- | The token for the next set of results, or ''null'' if there are no additional results.
lgNextToken :: Lens' ListGroups (Maybe Text)
lgNextToken = lens _lgNextToken (\ s a -> s{_lgNextToken = a})

-- | The maximum number of results to be returned per request.
lgMaxResults :: Lens' ListGroups (Maybe Text)
lgMaxResults = lens _lgMaxResults (\ s a -> s{_lgMaxResults = a})

instance AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupsResponse' <$>
                   (x .?> "Groups" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGroups where

instance NFData ListGroups where

instance ToHeaders ListGroups where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListGroups where
        toPath = const "/greengrass/groups"

instance ToQuery ListGroups where
        toQuery ListGroups'{..}
          = mconcat
              ["NextToken" =: _lgNextToken,
               "MaxResults" =: _lgMaxResults]

-- | /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { _lgrsGroups         :: !(Maybe [GroupInformation])
  , _lgrsNextToken      :: !(Maybe Text)
  , _lgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsGroups' - Information about a group.
--
-- * 'lgrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
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


-- | Information about a group.
lgrsGroups :: Lens' ListGroupsResponse [GroupInformation]
lgrsGroups = lens _lgrsGroups (\ s a -> s{_lgrsGroups = a}) . _Default . _Coerce

-- | The token for the next set of results, or ''null'' if there are no additional results.
lgrsNextToken :: Lens' ListGroupsResponse (Maybe Text)
lgrsNextToken = lens _lgrsNextToken (\ s a -> s{_lgrsNextToken = a})

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGroupsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a})

instance NFData ListGroupsResponse where
