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
-- Module      : Network.AWS.APIGateway.GetVPCLinks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'VpcLinks' collection under the caller's account in a selected region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetVPCLinks
    (
    -- * Creating a Request
      getVPCLinks
    , GetVPCLinks
    -- * Request Lenses
    , gvlLimit
    , gvlPosition

    -- * Destructuring the Response
    , getVPCLinksResponse
    , GetVPCLinksResponse
    -- * Response Lenses
    , gvlrsItems
    , gvlrsPosition
    , gvlrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Gets the 'VpcLinks' collection under the caller's account in a selected region.
--
--
--
-- /See:/ 'getVPCLinks' smart constructor.
data GetVPCLinks = GetVPCLinks'
  { _gvlLimit    :: !(Maybe Int)
  , _gvlPosition :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVPCLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvlLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gvlPosition' - The current pagination position in the paged result set.
getVPCLinks
    :: GetVPCLinks
getVPCLinks = GetVPCLinks' {_gvlLimit = Nothing, _gvlPosition = Nothing}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gvlLimit :: Lens' GetVPCLinks (Maybe Int)
gvlLimit = lens _gvlLimit (\ s a -> s{_gvlLimit = a})

-- | The current pagination position in the paged result set.
gvlPosition :: Lens' GetVPCLinks (Maybe Text)
gvlPosition = lens _gvlPosition (\ s a -> s{_gvlPosition = a})

instance AWSPager GetVPCLinks where
        page rq rs
          | stop (rs ^. gvlrsPosition) = Nothing
          | stop (rs ^. gvlrsItems) = Nothing
          | otherwise =
            Just $ rq & gvlPosition .~ rs ^. gvlrsPosition

instance AWSRequest GetVPCLinks where
        type Rs GetVPCLinks = GetVPCLinksResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetVPCLinksResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetVPCLinks where

instance NFData GetVPCLinks where

instance ToHeaders GetVPCLinks where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetVPCLinks where
        toPath = const "/vpclinks"

instance ToQuery GetVPCLinks where
        toQuery GetVPCLinks'{..}
          = mconcat
              ["limit" =: _gvlLimit, "position" =: _gvlPosition]

-- | The collection of VPC links under the caller's account in a region.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-with-private-integration.html Getting Started with Private Integrations> , <http://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-private-integration.html Set up Private Integrations>
--
-- /See:/ 'getVPCLinksResponse' smart constructor.
data GetVPCLinksResponse = GetVPCLinksResponse'
  { _gvlrsItems          :: !(Maybe [VPCLink])
  , _gvlrsPosition       :: !(Maybe Text)
  , _gvlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVPCLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvlrsItems' - The current page of elements from this collection.
--
-- * 'gvlrsPosition' - Undocumented member.
--
-- * 'gvlrsResponseStatus' - -- | The response status code.
getVPCLinksResponse
    :: Int -- ^ 'gvlrsResponseStatus'
    -> GetVPCLinksResponse
getVPCLinksResponse pResponseStatus_ =
  GetVPCLinksResponse'
    { _gvlrsItems = Nothing
    , _gvlrsPosition = Nothing
    , _gvlrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gvlrsItems :: Lens' GetVPCLinksResponse [VPCLink]
gvlrsItems = lens _gvlrsItems (\ s a -> s{_gvlrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gvlrsPosition :: Lens' GetVPCLinksResponse (Maybe Text)
gvlrsPosition = lens _gvlrsPosition (\ s a -> s{_gvlrsPosition = a})

-- | -- | The response status code.
gvlrsResponseStatus :: Lens' GetVPCLinksResponse Int
gvlrsResponseStatus = lens _gvlrsResponseStatus (\ s a -> s{_gvlrsResponseStatus = a})

instance NFData GetVPCLinksResponse where
