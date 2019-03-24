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
    , ubEngineVersion
    , ubAutoMinorVersionUpgrade
    , ubLogs
    , ubConfiguration
    , ubBrokerId

    -- * Destructuring the Response
    , updateBrokerResponse
    , UpdateBrokerResponse
    -- * Response Lenses
    , ubrsEngineVersion
    , ubrsAutoMinorVersionUpgrade
    , ubrsLogs
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
  { _ubEngineVersion           :: !(Maybe Text)
  , _ubAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _ubLogs                    :: !(Maybe Logs)
  , _ubConfiguration           :: !(Maybe ConfigurationId)
  , _ubBrokerId                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBroker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubEngineVersion' - The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- * 'ubAutoMinorVersionUpgrade' - Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
--
-- * 'ubLogs' - Enables Amazon CloudWatch logging for brokers.
--
-- * 'ubConfiguration' - A list of information about the configuration.
--
-- * 'ubBrokerId' - The name of the broker. This value must be unique in your AWS account, 1-50 characters long, must contain only letters, numbers, dashes, and underscores, and must not contain whitespaces, brackets, wildcard characters, or special characters.
updateBroker
    :: Text -- ^ 'ubBrokerId'
    -> UpdateBroker
updateBroker pBrokerId_ =
  UpdateBroker'
    { _ubEngineVersion = Nothing
    , _ubAutoMinorVersionUpgrade = Nothing
    , _ubLogs = Nothing
    , _ubConfiguration = Nothing
    , _ubBrokerId = pBrokerId_
    }


-- | The version of the broker engine. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
ubEngineVersion :: Lens' UpdateBroker (Maybe Text)
ubEngineVersion = lens _ubEngineVersion (\ s a -> s{_ubEngineVersion = a})

-- | Enables automatic upgrades to new minor versions for brokers, as Apache releases the versions. The automatic upgrades occur during the maintenance window of the broker or after a manual broker reboot.
ubAutoMinorVersionUpgrade :: Lens' UpdateBroker (Maybe Bool)
ubAutoMinorVersionUpgrade = lens _ubAutoMinorVersionUpgrade (\ s a -> s{_ubAutoMinorVersionUpgrade = a})

-- | Enables Amazon CloudWatch logging for brokers.
ubLogs :: Lens' UpdateBroker (Maybe Logs)
ubLogs = lens _ubLogs (\ s a -> s{_ubLogs = a})

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
                   (x .?> "engineVersion") <*>
                     (x .?> "autoMinorVersionUpgrade")
                     <*> (x .?> "logs")
                     <*> (x .?> "configuration")
                     <*> (x .?> "brokerId")
                     <*> (pure (fromEnum s)))

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
                 [("engineVersion" .=) <$> _ubEngineVersion,
                  ("autoMinorVersionUpgrade" .=) <$>
                    _ubAutoMinorVersionUpgrade,
                  ("logs" .=) <$> _ubLogs,
                  ("configuration" .=) <$> _ubConfiguration])

instance ToPath UpdateBroker where
        toPath UpdateBroker'{..}
          = mconcat ["/v1/brokers/", toBS _ubBrokerId]

instance ToQuery UpdateBroker where
        toQuery = const mempty

-- | /See:/ 'updateBrokerResponse' smart constructor.
data UpdateBrokerResponse = UpdateBrokerResponse'
  { _ubrsEngineVersion           :: !(Maybe Text)
  , _ubrsAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _ubrsLogs                    :: !(Maybe Logs)
  , _ubrsConfiguration           :: !(Maybe ConfigurationId)
  , _ubrsBrokerId                :: !(Maybe Text)
  , _ubrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBrokerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsEngineVersion' - The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
--
-- * 'ubrsAutoMinorVersionUpgrade' - The new value of automatic upgrades to new minor version for brokers.
--
-- * 'ubrsLogs' - The list of information about logs to be enabled for the specified broker.
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
    { _ubrsEngineVersion = Nothing
    , _ubrsAutoMinorVersionUpgrade = Nothing
    , _ubrsLogs = Nothing
    , _ubrsConfiguration = Nothing
    , _ubrsBrokerId = Nothing
    , _ubrsResponseStatus = pResponseStatus_
    }


-- | The version of the broker engine to upgrade to. For a list of supported engine versions, see https://docs.aws.amazon.com/amazon-mq/latest/developer-guide/broker-engine.html
ubrsEngineVersion :: Lens' UpdateBrokerResponse (Maybe Text)
ubrsEngineVersion = lens _ubrsEngineVersion (\ s a -> s{_ubrsEngineVersion = a})

-- | The new value of automatic upgrades to new minor version for brokers.
ubrsAutoMinorVersionUpgrade :: Lens' UpdateBrokerResponse (Maybe Bool)
ubrsAutoMinorVersionUpgrade = lens _ubrsAutoMinorVersionUpgrade (\ s a -> s{_ubrsAutoMinorVersionUpgrade = a})

-- | The list of information about logs to be enabled for the specified broker.
ubrsLogs :: Lens' UpdateBrokerResponse (Maybe Logs)
ubrsLogs = lens _ubrsLogs (\ s a -> s{_ubrsLogs = a})

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
