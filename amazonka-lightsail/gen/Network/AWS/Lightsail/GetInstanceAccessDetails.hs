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
-- Module      : Network.AWS.Lightsail.GetInstanceAccessDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns temporary SSH keys you can use to connect to a specific virtual private server, or /instance/ .
--
--
module Network.AWS.Lightsail.GetInstanceAccessDetails
    (
    -- * Creating a Request
      getInstanceAccessDetails
    , GetInstanceAccessDetails
    -- * Request Lenses
    , giadProtocol
    , giadInstanceName

    -- * Destructuring the Response
    , getInstanceAccessDetailsResponse
    , GetInstanceAccessDetailsResponse
    -- * Response Lenses
    , giadrsAccessDetails
    , giadrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceAccessDetails' smart constructor.
data GetInstanceAccessDetails = GetInstanceAccessDetails'
  { _giadProtocol     :: !(Maybe InstanceAccessProtocol)
  , _giadInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceAccessDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giadProtocol' - The protocol to use to connect to your instance. Defaults to @ssh@ .
--
-- * 'giadInstanceName' - The name of the instance to access.
getInstanceAccessDetails
    :: Text -- ^ 'giadInstanceName'
    -> GetInstanceAccessDetails
getInstanceAccessDetails pInstanceName_ =
  GetInstanceAccessDetails'
    {_giadProtocol = Nothing, _giadInstanceName = pInstanceName_}


-- | The protocol to use to connect to your instance. Defaults to @ssh@ .
giadProtocol :: Lens' GetInstanceAccessDetails (Maybe InstanceAccessProtocol)
giadProtocol = lens _giadProtocol (\ s a -> s{_giadProtocol = a})

-- | The name of the instance to access.
giadInstanceName :: Lens' GetInstanceAccessDetails Text
giadInstanceName = lens _giadInstanceName (\ s a -> s{_giadInstanceName = a})

instance AWSRequest GetInstanceAccessDetails where
        type Rs GetInstanceAccessDetails =
             GetInstanceAccessDetailsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceAccessDetailsResponse' <$>
                   (x .?> "accessDetails") <*> (pure (fromEnum s)))

instance Hashable GetInstanceAccessDetails where

instance NFData GetInstanceAccessDetails where

instance ToHeaders GetInstanceAccessDetails where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstanceAccessDetails" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceAccessDetails where
        toJSON GetInstanceAccessDetails'{..}
          = object
              (catMaybes
                 [("protocol" .=) <$> _giadProtocol,
                  Just ("instanceName" .= _giadInstanceName)])

instance ToPath GetInstanceAccessDetails where
        toPath = const "/"

instance ToQuery GetInstanceAccessDetails where
        toQuery = const mempty

-- | /See:/ 'getInstanceAccessDetailsResponse' smart constructor.
data GetInstanceAccessDetailsResponse = GetInstanceAccessDetailsResponse'
  { _giadrsAccessDetails  :: !(Maybe InstanceAccessDetails)
  , _giadrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceAccessDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giadrsAccessDetails' - An array of key-value pairs containing information about a get instance access request.
--
-- * 'giadrsResponseStatus' - -- | The response status code.
getInstanceAccessDetailsResponse
    :: Int -- ^ 'giadrsResponseStatus'
    -> GetInstanceAccessDetailsResponse
getInstanceAccessDetailsResponse pResponseStatus_ =
  GetInstanceAccessDetailsResponse'
    {_giadrsAccessDetails = Nothing, _giadrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about a get instance access request.
giadrsAccessDetails :: Lens' GetInstanceAccessDetailsResponse (Maybe InstanceAccessDetails)
giadrsAccessDetails = lens _giadrsAccessDetails (\ s a -> s{_giadrsAccessDetails = a})

-- | -- | The response status code.
giadrsResponseStatus :: Lens' GetInstanceAccessDetailsResponse Int
giadrsResponseStatus = lens _giadrsResponseStatus (\ s a -> s{_giadrsResponseStatus = a})

instance NFData GetInstanceAccessDetailsResponse
         where
