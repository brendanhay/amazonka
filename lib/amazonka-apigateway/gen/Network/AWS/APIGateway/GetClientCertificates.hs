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
-- Module      : Network.AWS.APIGateway.GetClientCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of 'ClientCertificate' resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetClientCertificates
    (
    -- * Creating a Request
      getClientCertificates
    , GetClientCertificates
    -- * Request Lenses
    , gccLimit
    , gccPosition

    -- * Destructuring the Response
    , getClientCertificatesResponse
    , GetClientCertificatesResponse
    -- * Response Lenses
    , gccrsItems
    , gccrsPosition
    , gccrsResponseStatus
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to get information about a collection of 'ClientCertificate' resources.
--
--
--
-- /See:/ 'getClientCertificates' smart constructor.
data GetClientCertificates = GetClientCertificates'
  { _gccLimit    :: !(Maybe Int)
  , _gccPosition :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClientCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccLimit' - The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- * 'gccPosition' - The current pagination position in the paged result set.
getClientCertificates
    :: GetClientCertificates
getClientCertificates =
  GetClientCertificates' {_gccLimit = Nothing, _gccPosition = Nothing}


-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
gccLimit :: Lens' GetClientCertificates (Maybe Int)
gccLimit = lens _gccLimit (\ s a -> s{_gccLimit = a})

-- | The current pagination position in the paged result set.
gccPosition :: Lens' GetClientCertificates (Maybe Text)
gccPosition = lens _gccPosition (\ s a -> s{_gccPosition = a})

instance AWSPager GetClientCertificates where
        page rq rs
          | stop (rs ^. gccrsPosition) = Nothing
          | stop (rs ^. gccrsItems) = Nothing
          | otherwise =
            Just $ rq & gccPosition .~ rs ^. gccrsPosition

instance AWSRequest GetClientCertificates where
        type Rs GetClientCertificates =
             GetClientCertificatesResponse
        request = get apiGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetClientCertificatesResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetClientCertificates where

instance NFData GetClientCertificates where

instance ToHeaders GetClientCertificates where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetClientCertificates where
        toPath = const "/clientcertificates"

instance ToQuery GetClientCertificates where
        toQuery GetClientCertificates'{..}
          = mconcat
              ["limit" =: _gccLimit, "position" =: _gccPosition]

-- | Represents a collection of 'ClientCertificate' resources.
--
--
-- <http://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate>
--
-- /See:/ 'getClientCertificatesResponse' smart constructor.
data GetClientCertificatesResponse = GetClientCertificatesResponse'
  { _gccrsItems          :: !(Maybe [ClientCertificate])
  , _gccrsPosition       :: !(Maybe Text)
  , _gccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetClientCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccrsItems' - The current page of elements from this collection.
--
-- * 'gccrsPosition' - Undocumented member.
--
-- * 'gccrsResponseStatus' - -- | The response status code.
getClientCertificatesResponse
    :: Int -- ^ 'gccrsResponseStatus'
    -> GetClientCertificatesResponse
getClientCertificatesResponse pResponseStatus_ =
  GetClientCertificatesResponse'
    { _gccrsItems = Nothing
    , _gccrsPosition = Nothing
    , _gccrsResponseStatus = pResponseStatus_
    }


-- | The current page of elements from this collection.
gccrsItems :: Lens' GetClientCertificatesResponse [ClientCertificate]
gccrsItems = lens _gccrsItems (\ s a -> s{_gccrsItems = a}) . _Default . _Coerce

-- | Undocumented member.
gccrsPosition :: Lens' GetClientCertificatesResponse (Maybe Text)
gccrsPosition = lens _gccrsPosition (\ s a -> s{_gccrsPosition = a})

-- | -- | The response status code.
gccrsResponseStatus :: Lens' GetClientCertificatesResponse Int
gccrsResponseStatus = lens _gccrsResponseStatus (\ s a -> s{_gccrsResponseStatus = a})

instance NFData GetClientCertificatesResponse where
