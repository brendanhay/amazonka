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
-- Module      : Network.AWS.MQ.UpdateBroker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a pending configuration change to a broker.
module Network.AWS.MQ.UpdateBroker
    (
    -- * Creating a Request
      updateBroker
    , UpdateBroker
    -- * Request Lenses
    , ubConfiguration
    , ubBrokerId

    -- * Destructuring the Response
    , updateBrokerResponse
    , UpdateBrokerResponse
    -- * Response Lenses
    , ubrsConfiguration
    , ubrsBrokerId
    , ubrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Updates the broker using the specified properties.
--
-- /See:/ 'updateBroker' smart constructor.
data UpdateBroker = UpdateBroker'
  { _ubConfiguration :: !(Maybe ConfigurationId)
  , _ubBrokerId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBroker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubConfiguration' - A list of information about the configuration.
--
-- * 'ubBrokerId' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
updateBroker
    :: Text -- ^ 'ubBrokerId'
    -> UpdateBroker
updateBroker pBrokerId_ =
  UpdateBroker' {_ubConfiguration = Nothing, _ubBrokerId = pBrokerId_}


-- | A list of information about the configuration.
ubConfiguration :: Lens' UpdateBroker (Maybe ConfigurationId)
ubConfiguration = lens _ubConfiguration (\ s a -> s{_ubConfiguration = a})

-- | The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
ubBrokerId :: Lens' UpdateBroker Text
ubBrokerId = lens _ubBrokerId (\ s a -> s{_ubBrokerId = a})

instance AWSRequest UpdateBroker where
        type Rs UpdateBroker = UpdateBrokerResponse
        request = putJSON mq
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBrokerResponse' <$>
                   (x .?> "configuration") <*> (x .?> "brokerId") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateBroker where

instance NFData UpdateBroker where

instance ToHeaders UpdateBroker where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBroker where
        toJSON UpdateBroker'{..}
          = object
              (catMaybes
                 [("configuration" .=) <$> _ubConfiguration])

instance ToPath UpdateBroker where
        toPath UpdateBroker'{..}
          = mconcat ["/v1/brokers/", toBS _ubBrokerId]

instance ToQuery UpdateBroker where
        toQuery = const mempty

-- | /See:/ 'updateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { _ubrsConfiguration  :: !(Maybe ConfigurationId)
  , _ubrsBrokerId       :: !(Maybe Text)
  , _ubrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBrokerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsConfiguration' - The ID of the updated configuration.
--
-- * 'ubrsBrokerId' - Required. The unique ID that Amazon MQ generates for the broker.
--
-- * 'ubrsResponseStatus' - -- | The response status code.
updateBrokerResponse
    :: Int -- ^ 'ubrsResponseStatus'
    -> UpdateBrokerResponse
updateBrokerResponse pResponseStatus_ =
  UpdateBrokerResponse'
    { _ubrsConfiguration = Nothing
    , _ubrsBrokerId = Nothing
    , _ubrsResponseStatus = pResponseStatus_
    }


-- | The ID of the updated configuration.
ubrsConfiguration :: Lens' UpdateBrokerResponse (Maybe ConfigurationId)
ubrsConfiguration = lens _ubrsConfiguration (\ s a -> s{_ubrsConfiguration = a})

-- | Required. The unique ID that Amazon MQ generates for the broker.
ubrsBrokerId :: Lens' UpdateBrokerResponse (Maybe Text)
ubrsBrokerId = lens _ubrsBrokerId (\ s a -> s{_ubrsBrokerId = a})

-- | -- | The response status code.
ubrsResponseStatus :: Lens' UpdateBrokerResponse Int
ubrsResponseStatus = lens _ubrsResponseStatus (\ s a -> s{_ubrsResponseStatus = a})

instance NFData UpdateBrokerResponse where
