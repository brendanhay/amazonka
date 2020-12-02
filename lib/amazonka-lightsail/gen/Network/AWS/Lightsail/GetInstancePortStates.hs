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
-- Module      : Network.AWS.Lightsail.GetInstancePortStates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the port states for a specific virtual private server, or /instance/ .
--
--
module Network.AWS.Lightsail.GetInstancePortStates
    (
    -- * Creating a Request
      getInstancePortStates
    , GetInstancePortStates
    -- * Request Lenses
    , gipsInstanceName

    -- * Destructuring the Response
    , getInstancePortStatesResponse
    , GetInstancePortStatesResponse
    -- * Response Lenses
    , gipsrsPortStates
    , gipsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstancePortStates' smart constructor.
newtype GetInstancePortStates = GetInstancePortStates'
  { _gipsInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstancePortStates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipsInstanceName' - The name of the instance.
getInstancePortStates
    :: Text -- ^ 'gipsInstanceName'
    -> GetInstancePortStates
getInstancePortStates pInstanceName_ =
  GetInstancePortStates' {_gipsInstanceName = pInstanceName_}


-- | The name of the instance.
gipsInstanceName :: Lens' GetInstancePortStates Text
gipsInstanceName = lens _gipsInstanceName (\ s a -> s{_gipsInstanceName = a})

instance AWSRequest GetInstancePortStates where
        type Rs GetInstancePortStates =
             GetInstancePortStatesResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstancePortStatesResponse' <$>
                   (x .?> "portStates" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetInstancePortStates where

instance NFData GetInstancePortStates where

instance ToHeaders GetInstancePortStates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstancePortStates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstancePortStates where
        toJSON GetInstancePortStates'{..}
          = object
              (catMaybes
                 [Just ("instanceName" .= _gipsInstanceName)])

instance ToPath GetInstancePortStates where
        toPath = const "/"

instance ToQuery GetInstancePortStates where
        toQuery = const mempty

-- | /See:/ 'getInstancePortStatesResponse' smart constructor.
data GetInstancePortStatesResponse = GetInstancePortStatesResponse'
  { _gipsrsPortStates     :: !(Maybe [InstancePortState])
  , _gipsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstancePortStatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gipsrsPortStates' - Information about the port states resulting from your request.
--
-- * 'gipsrsResponseStatus' - -- | The response status code.
getInstancePortStatesResponse
    :: Int -- ^ 'gipsrsResponseStatus'
    -> GetInstancePortStatesResponse
getInstancePortStatesResponse pResponseStatus_ =
  GetInstancePortStatesResponse'
    {_gipsrsPortStates = Nothing, _gipsrsResponseStatus = pResponseStatus_}


-- | Information about the port states resulting from your request.
gipsrsPortStates :: Lens' GetInstancePortStatesResponse [InstancePortState]
gipsrsPortStates = lens _gipsrsPortStates (\ s a -> s{_gipsrsPortStates = a}) . _Default . _Coerce

-- | -- | The response status code.
gipsrsResponseStatus :: Lens' GetInstancePortStatesResponse Int
gipsrsResponseStatus = lens _gipsrsResponseStatus (\ s a -> s{_gipsrsResponseStatus = a})

instance NFData GetInstancePortStatesResponse where
