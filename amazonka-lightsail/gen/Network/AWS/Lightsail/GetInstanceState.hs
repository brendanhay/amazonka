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
-- Module      : Network.AWS.Lightsail.GetInstanceState
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the state of a specific instance. Works on one instance at a time.
--
--
module Network.AWS.Lightsail.GetInstanceState
    (
    -- * Creating a Request
      getInstanceState
    , GetInstanceState
    -- * Request Lenses
    , gisInstanceName

    -- * Destructuring the Response
    , getInstanceStateResponse
    , GetInstanceStateResponse
    -- * Response Lenses
    , getrsState
    , getrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceState' smart constructor.
newtype GetInstanceState = GetInstanceState'
  { _gisInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisInstanceName' - The name of the instance to get state information about.
getInstanceState
    :: Text -- ^ 'gisInstanceName'
    -> GetInstanceState
getInstanceState pInstanceName_ =
  GetInstanceState' {_gisInstanceName = pInstanceName_}


-- | The name of the instance to get state information about.
gisInstanceName :: Lens' GetInstanceState Text
gisInstanceName = lens _gisInstanceName (\ s a -> s{_gisInstanceName = a});

instance AWSRequest GetInstanceState where
        type Rs GetInstanceState = GetInstanceStateResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceStateResponse' <$>
                   (x .?> "state") <*> (pure (fromEnum s)))

instance Hashable GetInstanceState where

instance NFData GetInstanceState where

instance ToHeaders GetInstanceState where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstanceState" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceState where
        toJSON GetInstanceState'{..}
          = object
              (catMaybes
                 [Just ("instanceName" .= _gisInstanceName)])

instance ToPath GetInstanceState where
        toPath = const "/"

instance ToQuery GetInstanceState where
        toQuery = const mempty

-- | /See:/ 'getInstanceStateResponse' smart constructor.
data GetInstanceStateResponse = GetInstanceStateResponse'
  { _getrsState          :: !(Maybe InstanceState)
  , _getrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsState' - The state of the instance.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getInstanceStateResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> GetInstanceStateResponse
getInstanceStateResponse pResponseStatus_ =
  GetInstanceStateResponse'
  {_getrsState = Nothing, _getrsResponseStatus = pResponseStatus_}


-- | The state of the instance.
getrsState :: Lens' GetInstanceStateResponse (Maybe InstanceState)
getrsState = lens _getrsState (\ s a -> s{_getrsState = a});

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetInstanceStateResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a});

instance NFData GetInstanceStateResponse where
