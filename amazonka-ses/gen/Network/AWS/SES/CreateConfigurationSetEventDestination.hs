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
-- Module      : Network.AWS.SES.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set event destination.
--
--
-- An event destination is the AWS service to which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.CreateConfigurationSetEventDestination
    (
    -- * Creating a Request
      createConfigurationSetEventDestination
    , CreateConfigurationSetEventDestination
    -- * Request Lenses
    , ccsedConfigurationSetName
    , ccsedEventDestination

    -- * Destructuring the Response
    , createConfigurationSetEventDestinationResponse
    , CreateConfigurationSetEventDestinationResponse
    -- * Response Lenses
    , ccsedrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to create a configuration set event destination. A configuration set event destination, which can be either Amazon CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which Amazon SES publishes the email sending events associated with a configuration set. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'createConfigurationSetEventDestination' smart constructor.
data CreateConfigurationSetEventDestination = CreateConfigurationSetEventDestination'
  { _ccsedConfigurationSetName :: !Text
  , _ccsedEventDestination     :: !EventDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfigurationSetEventDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsedConfigurationSetName' - The name of the configuration set that the event destination should be associated with.
--
-- * 'ccsedEventDestination' - An object that describes the AWS service that email sending event information will be published to.
createConfigurationSetEventDestination
    :: Text -- ^ 'ccsedConfigurationSetName'
    -> EventDestination -- ^ 'ccsedEventDestination'
    -> CreateConfigurationSetEventDestination
createConfigurationSetEventDestination pConfigurationSetName_ pEventDestination_ =
  CreateConfigurationSetEventDestination'
    { _ccsedConfigurationSetName = pConfigurationSetName_
    , _ccsedEventDestination = pEventDestination_
    }


-- | The name of the configuration set that the event destination should be associated with.
ccsedConfigurationSetName :: Lens' CreateConfigurationSetEventDestination Text
ccsedConfigurationSetName = lens _ccsedConfigurationSetName (\ s a -> s{_ccsedConfigurationSetName = a})

-- | An object that describes the AWS service that email sending event information will be published to.
ccsedEventDestination :: Lens' CreateConfigurationSetEventDestination EventDestination
ccsedEventDestination = lens _ccsedEventDestination (\ s a -> s{_ccsedEventDestination = a})

instance AWSRequest
           CreateConfigurationSetEventDestination
         where
        type Rs CreateConfigurationSetEventDestination =
             CreateConfigurationSetEventDestinationResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "CreateConfigurationSetEventDestinationResult"
              (\ s h x ->
                 CreateConfigurationSetEventDestinationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           CreateConfigurationSetEventDestination
         where

instance NFData
           CreateConfigurationSetEventDestination
         where

instance ToHeaders
           CreateConfigurationSetEventDestination
         where
        toHeaders = const mempty

instance ToPath
           CreateConfigurationSetEventDestination
         where
        toPath = const "/"

instance ToQuery
           CreateConfigurationSetEventDestination
         where
        toQuery CreateConfigurationSetEventDestination'{..}
          = mconcat
              ["Action" =:
                 ("CreateConfigurationSetEventDestination" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _ccsedConfigurationSetName,
               "EventDestination" =: _ccsedEventDestination]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'createConfigurationSetEventDestinationResponse' smart constructor.
newtype CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse'
  { _ccsedrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateConfigurationSetEventDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsedrsResponseStatus' - -- | The response status code.
createConfigurationSetEventDestinationResponse
    :: Int -- ^ 'ccsedrsResponseStatus'
    -> CreateConfigurationSetEventDestinationResponse
createConfigurationSetEventDestinationResponse pResponseStatus_ =
  CreateConfigurationSetEventDestinationResponse'
    {_ccsedrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ccsedrsResponseStatus :: Lens' CreateConfigurationSetEventDestinationResponse Int
ccsedrsResponseStatus = lens _ccsedrsResponseStatus (\ s a -> s{_ccsedrsResponseStatus = a})

instance NFData
           CreateConfigurationSetEventDestinationResponse
         where
