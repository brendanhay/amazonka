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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
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

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getClientCertificates' smart constructor.
data GetClientCertificates = GetClientCertificates'
    { _gccLimit    :: !(Maybe Int)
    , _gccPosition :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetClientCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccLimit'
--
-- * 'gccPosition'
getClientCertificates
    :: GetClientCertificates
getClientCertificates =
    GetClientCertificates'
    { _gccLimit = Nothing
    , _gccPosition = Nothing
    }

-- | Undocumented member.
gccLimit :: Lens' GetClientCertificates (Maybe Int)
gccLimit = lens _gccLimit (\ s a -> s{_gccLimit = a});

-- | Undocumented member.
gccPosition :: Lens' GetClientCertificates (Maybe Text)
gccPosition = lens _gccPosition (\ s a -> s{_gccPosition = a});

instance AWSPager GetClientCertificates where
        page rq rs
          | stop (rs ^. gccrsPosition) = Nothing
          | stop (rs ^. gccrsItems) = Nothing
          | otherwise =
            Just $ rq & gccPosition .~ rs ^. gccrsPosition

instance AWSRequest GetClientCertificates where
        type Rs GetClientCertificates =
             GetClientCertificatesResponse
        request = get aPIGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetClientCertificatesResponse' <$>
                   (x .?> "item" .!@ mempty) <*> (x .?> "position") <*>
                     (pure (fromEnum s)))

instance Hashable GetClientCertificates

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

-- | /See:/ 'getClientCertificatesResponse' smart constructor.
data GetClientCertificatesResponse = GetClientCertificatesResponse'
    { _gccrsItems          :: !(Maybe [ClientCertificate])
    , _gccrsPosition       :: !(Maybe Text)
    , _gccrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetClientCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gccrsItems'
--
-- * 'gccrsPosition'
--
-- * 'gccrsResponseStatus'
getClientCertificatesResponse
    :: Int -- ^ 'gccrsResponseStatus'
    -> GetClientCertificatesResponse
getClientCertificatesResponse pResponseStatus_ =
    GetClientCertificatesResponse'
    { _gccrsItems = Nothing
    , _gccrsPosition = Nothing
    , _gccrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gccrsItems :: Lens' GetClientCertificatesResponse [ClientCertificate]
gccrsItems = lens _gccrsItems (\ s a -> s{_gccrsItems = a}) . _Default . _Coerce;

-- | Undocumented member.
gccrsPosition :: Lens' GetClientCertificatesResponse (Maybe Text)
gccrsPosition = lens _gccrsPosition (\ s a -> s{_gccrsPosition = a});

-- | The response status code.
gccrsResponseStatus :: Lens' GetClientCertificatesResponse Int
gccrsResponseStatus = lens _gccrsResponseStatus (\ s a -> s{_gccrsResponseStatus = a});
