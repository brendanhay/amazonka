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
-- Module      : Network.AWS.Lightsail.StartInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific Amazon Lightsail instance from a stopped state. To restart an instance, use the reboot instance operation.
--
--
module Network.AWS.Lightsail.StartInstance
    (
    -- * Creating a Request
      startInstance
    , StartInstance
    -- * Request Lenses
    , sInstanceName

    -- * Destructuring the Response
    , startInstanceResponse
    , StartInstanceResponse
    -- * Response Lenses
    , srsOperations
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startInstance' smart constructor.
newtype StartInstance = StartInstance'
  { _sInstanceName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sInstanceName' - The name of the instance (a virtual private server) to start.
startInstance
    :: Text -- ^ 'sInstanceName'
    -> StartInstance
startInstance pInstanceName_ = StartInstance' {_sInstanceName = pInstanceName_}


-- | The name of the instance (a virtual private server) to start.
sInstanceName :: Lens' StartInstance Text
sInstanceName = lens _sInstanceName (\ s a -> s{_sInstanceName = a})

instance AWSRequest StartInstance where
        type Rs StartInstance = StartInstanceResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 StartInstanceResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StartInstance where

instance NFData StartInstance where

instance ToHeaders StartInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.StartInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartInstance where
        toJSON StartInstance'{..}
          = object
              (catMaybes [Just ("instanceName" .= _sInstanceName)])

instance ToPath StartInstance where
        toPath = const "/"

instance ToQuery StartInstance where
        toQuery = const mempty

-- | /See:/ 'startInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  { _srsOperations     :: !(Maybe [Operation])
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsOperations' - An array of key-value pairs containing information about the request operation.
--
-- * 'srsResponseStatus' - -- | The response status code.
startInstanceResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartInstanceResponse
startInstanceResponse pResponseStatus_ =
  StartInstanceResponse'
    {_srsOperations = Nothing, _srsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
srsOperations :: Lens' StartInstanceResponse [Operation]
srsOperations = lens _srsOperations (\ s a -> s{_srsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
srsResponseStatus :: Lens' StartInstanceResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartInstanceResponse where
