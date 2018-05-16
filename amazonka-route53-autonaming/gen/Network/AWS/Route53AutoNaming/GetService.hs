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
-- Module      : Network.AWS.Route53AutoNaming.GetService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the settings for a specified service.
--
--
module Network.AWS.Route53AutoNaming.GetService
    (
    -- * Creating a Request
      getService
    , GetService
    -- * Request Lenses
    , gsId

    -- * Destructuring the Response
    , getServiceResponse
    , GetServiceResponse
    -- * Response Lenses
    , gsrsService
    , gsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'getService' smart constructor.
newtype GetService = GetService'
  { _gsId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsId' - The ID of the service that you want to get settings for.
getService
    :: Text -- ^ 'gsId'
    -> GetService
getService pId_ = GetService' {_gsId = pId_}


-- | The ID of the service that you want to get settings for.
gsId :: Lens' GetService Text
gsId = lens _gsId (\ s a -> s{_gsId = a})

instance AWSRequest GetService where
        type Rs GetService = GetServiceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 GetServiceResponse' <$>
                   (x .?> "Service") <*> (pure (fromEnum s)))

instance Hashable GetService where

instance NFData GetService where

instance ToHeaders GetService where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.GetService" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetService where
        toJSON GetService'{..}
          = object (catMaybes [Just ("Id" .= _gsId)])

instance ToPath GetService where
        toPath = const "/"

instance ToQuery GetService where
        toQuery = const mempty

-- | /See:/ 'getServiceResponse' smart constructor.
data GetServiceResponse = GetServiceResponse'
  { _gsrsService        :: !(Maybe ServiceInfo)
  , _gsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsService' - A complex type that contains information about the service.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getServiceResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> GetServiceResponse
getServiceResponse pResponseStatus_ =
  GetServiceResponse'
    {_gsrsService = Nothing, _gsrsResponseStatus = pResponseStatus_}


-- | A complex type that contains information about the service.
gsrsService :: Lens' GetServiceResponse (Maybe ServiceInfo)
gsrsService = lens _gsrsService (\ s a -> s{_gsrsService = a})

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetServiceResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a})

instance NFData GetServiceResponse where
