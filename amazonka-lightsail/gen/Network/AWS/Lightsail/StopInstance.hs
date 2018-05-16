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
-- Module      : Network.AWS.Lightsail.StopInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific Amazon Lightsail instance that is currently running.
--
--
module Network.AWS.Lightsail.StopInstance
    (
    -- * Creating a Request
      stopInstance
    , StopInstance
    -- * Request Lenses
    , siForce
    , siInstanceName

    -- * Destructuring the Response
    , stopInstanceResponse
    , StopInstanceResponse
    -- * Response Lenses
    , sirsOperations
    , sirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopInstance' smart constructor.
data StopInstance = StopInstance'
  { _siForce        :: !(Maybe Bool)
  , _siInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siForce' - When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop. /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
--
-- * 'siInstanceName' - The name of the instance (a virtual private server) to stop.
stopInstance
    :: Text -- ^ 'siInstanceName'
    -> StopInstance
stopInstance pInstanceName_ =
  StopInstance' {_siForce = Nothing, _siInstanceName = pInstanceName_}


-- | When set to @True@ , forces a Lightsail instance that is stuck in a @stopping@ state to stop. /Important:/ Only use the @force@ parameter if your instance is stuck in the @stopping@ state. In any other state, your instance should stop normally without adding this parameter to your API request.
siForce :: Lens' StopInstance (Maybe Bool)
siForce = lens _siForce (\ s a -> s{_siForce = a})

-- | The name of the instance (a virtual private server) to stop.
siInstanceName :: Lens' StopInstance Text
siInstanceName = lens _siInstanceName (\ s a -> s{_siInstanceName = a})

instance AWSRequest StopInstance where
        type Rs StopInstance = StopInstanceResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 StopInstanceResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StopInstance where

instance NFData StopInstance where

instance ToHeaders StopInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.StopInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopInstance where
        toJSON StopInstance'{..}
          = object
              (catMaybes
                 [("force" .=) <$> _siForce,
                  Just ("instanceName" .= _siInstanceName)])

instance ToPath StopInstance where
        toPath = const "/"

instance ToQuery StopInstance where
        toQuery = const mempty

-- | /See:/ 'stopInstanceResponse' smart constructor.
data StopInstanceResponse = StopInstanceResponse'
  { _sirsOperations     :: !(Maybe [Operation])
  , _sirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirsOperations' - An array of key-value pairs containing information about the request operation.
--
-- * 'sirsResponseStatus' - -- | The response status code.
stopInstanceResponse
    :: Int -- ^ 'sirsResponseStatus'
    -> StopInstanceResponse
stopInstanceResponse pResponseStatus_ =
  StopInstanceResponse'
    {_sirsOperations = Nothing, _sirsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
sirsOperations :: Lens' StopInstanceResponse [Operation]
sirsOperations = lens _sirsOperations (\ s a -> s{_sirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
sirsResponseStatus :: Lens' StopInstanceResponse Int
sirsResponseStatus = lens _sirsResponseStatus (\ s a -> s{_sirsResponseStatus = a})

instance NFData StopInstanceResponse where
