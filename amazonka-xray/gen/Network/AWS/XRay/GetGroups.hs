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
-- Module      : Network.AWS.XRay.GetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active group details.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetGroups
    (
    -- * Creating a Request
      getGroups
    , GetGroups
    -- * Request Lenses
    , ggNextToken

    -- * Destructuring the Response
    , getGroupsResponse
    , GetGroupsResponse
    -- * Response Lenses
    , grsGroups
    , grsNextToken
    , grsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getGroups' smart constructor.
newtype GetGroups = GetGroups'
  { _ggNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggNextToken' - Pagination token. Not used.
getGroups
    :: GetGroups
getGroups = GetGroups' {_ggNextToken = Nothing}


-- | Pagination token. Not used.
ggNextToken :: Lens' GetGroups (Maybe Text)
ggNextToken = lens _ggNextToken (\ s a -> s{_ggNextToken = a})

instance AWSPager GetGroups where
        page rq rs
          | stop (rs ^. grsNextToken) = Nothing
          | stop (rs ^. grsGroups) = Nothing
          | otherwise =
            Just $ rq & ggNextToken .~ rs ^. grsNextToken

instance AWSRequest GetGroups where
        type Rs GetGroups = GetGroupsResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupsResponse' <$>
                   (x .?> "Groups" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetGroups where

instance NFData GetGroups where

instance ToHeaders GetGroups where
        toHeaders = const mempty

instance ToJSON GetGroups where
        toJSON GetGroups'{..}
          = object
              (catMaybes [("NextToken" .=) <$> _ggNextToken])

instance ToPath GetGroups where
        toPath = const "/Groups"

instance ToQuery GetGroups where
        toQuery = const mempty

-- | /See:/ 'getGroupsResponse' smart constructor.
data GetGroupsResponse = GetGroupsResponse'
  { _grsGroups :: !(Maybe [GroupSummary])
  , _grsNextToken :: !(Maybe Text)
  , _grsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsGroups' - The collection of all active groups.
--
-- * 'grsNextToken' - Pagination token. Not used.
--
-- * 'grsResponseStatus' - -- | The response status code.
getGroupsResponse
    :: Int -- ^ 'grsResponseStatus'
    -> GetGroupsResponse
getGroupsResponse pResponseStatus_ =
  GetGroupsResponse'
    { _grsGroups = Nothing
    , _grsNextToken = Nothing
    , _grsResponseStatus = pResponseStatus_
    }


-- | The collection of all active groups.
grsGroups :: Lens' GetGroupsResponse [GroupSummary]
grsGroups = lens _grsGroups (\ s a -> s{_grsGroups = a}) . _Default . _Coerce

-- | Pagination token. Not used.
grsNextToken :: Lens' GetGroupsResponse (Maybe Text)
grsNextToken = lens _grsNextToken (\ s a -> s{_grsNextToken = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetGroupsResponse Int
grsResponseStatus = lens _grsResponseStatus (\ s a -> s{_grsResponseStatus = a})

instance NFData GetGroupsResponse where
