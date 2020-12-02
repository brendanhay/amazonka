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
-- Module      : Network.AWS.StorageGateway.ListGateways
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists gateways owned by an AWS account in a region specified in the request. The returned list is ordered by gateway Amazon Resource Name (ARN).
--
--
-- By default, the operation returns a maximum of 100 gateways. This operation supports pagination that allows you to optionally reduce the number of gateways returned in a response.
--
-- If you have more gateways than are returned in a response (that is, the response returns only a truncated list of your gateways), the response contains a marker that you can specify in your next request to fetch the next page of gateways.
--
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListGateways
    (
    -- * Creating a Request
      listGateways
    , ListGateways
    -- * Request Lenses
    , lgMarker
    , lgLimit

    -- * Destructuring the Response
    , listGatewaysResponse
    , ListGatewaysResponse
    -- * Response Lenses
    , lgrsMarker
    , lgrsGateways
    , lgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing zero or more of the following fields:
--
--
--     * 'ListGatewaysInput$Limit'
--
--     * 'ListGatewaysInput$Marker'
--
--
--
--
-- /See:/ 'listGateways' smart constructor.
data ListGateways = ListGateways'
  { _lgMarker :: !(Maybe Text)
  , _lgLimit  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgMarker' - An opaque string that indicates the position at which to begin the returned list of gateways.
--
-- * 'lgLimit' - Specifies that the list of gateways returned be limited to the specified number of items.
listGateways
    :: ListGateways
listGateways = ListGateways' {_lgMarker = Nothing, _lgLimit = Nothing}


-- | An opaque string that indicates the position at which to begin the returned list of gateways.
lgMarker :: Lens' ListGateways (Maybe Text)
lgMarker = lens _lgMarker (\ s a -> s{_lgMarker = a})

-- | Specifies that the list of gateways returned be limited to the specified number of items.
lgLimit :: Lens' ListGateways (Maybe Natural)
lgLimit = lens _lgLimit (\ s a -> s{_lgLimit = a}) . mapping _Nat

instance AWSPager ListGateways where
        page rq rs
          | stop (rs ^. lgrsMarker) = Nothing
          | stop (rs ^. lgrsGateways) = Nothing
          | otherwise =
            Just $ rq & lgMarker .~ rs ^. lgrsMarker

instance AWSRequest ListGateways where
        type Rs ListGateways = ListGatewaysResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListGatewaysResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Gateways" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListGateways where

instance NFData ListGateways where

instance ToHeaders ListGateways where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListGateways" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGateways where
        toJSON ListGateways'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lgMarker,
                  ("Limit" .=) <$> _lgLimit])

instance ToPath ListGateways where
        toPath = const "/"

instance ToQuery ListGateways where
        toQuery = const mempty

-- | /See:/ 'listGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { _lgrsMarker         :: !(Maybe Text)
  , _lgrsGateways       :: !(Maybe [GatewayInfo])
  , _lgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsMarker' - Undocumented member.
--
-- * 'lgrsGateways' - Undocumented member.
--
-- * 'lgrsResponseStatus' - -- | The response status code.
listGatewaysResponse
    :: Int -- ^ 'lgrsResponseStatus'
    -> ListGatewaysResponse
listGatewaysResponse pResponseStatus_ =
  ListGatewaysResponse'
    { _lgrsMarker = Nothing
    , _lgrsGateways = Nothing
    , _lgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lgrsMarker :: Lens' ListGatewaysResponse (Maybe Text)
lgrsMarker = lens _lgrsMarker (\ s a -> s{_lgrsMarker = a})

-- | Undocumented member.
lgrsGateways :: Lens' ListGatewaysResponse [GatewayInfo]
lgrsGateways = lens _lgrsGateways (\ s a -> s{_lgrsGateways = a}) . _Default . _Coerce

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGatewaysResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a})

instance NFData ListGatewaysResponse where
