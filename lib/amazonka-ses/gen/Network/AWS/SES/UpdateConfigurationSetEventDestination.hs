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
-- Module      : Network.AWS.SES.UpdateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event destination of a configuration set. Event destinations are associated with configuration sets, which enable you to publish email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS). For information about using configuration sets, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity> in the /Amazon SES Developer Guide./
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.UpdateConfigurationSetEventDestination
    (
    -- * Creating a Request
      updateConfigurationSetEventDestination
    , UpdateConfigurationSetEventDestination
    -- * Request Lenses
    , ucsedConfigurationSetName
    , ucsedEventDestination

    -- * Destructuring the Response
    , updateConfigurationSetEventDestinationResponse
    , UpdateConfigurationSetEventDestinationResponse
    -- * Response Lenses
    , ucsedrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to update the event destination of a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'updateConfigurationSetEventDestination' smart constructor.
data UpdateConfigurationSetEventDestination = UpdateConfigurationSetEventDestination'
  { _ucsedConfigurationSetName :: !Text
  , _ucsedEventDestination     :: !EventDestination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetEventDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsedConfigurationSetName' - The name of the configuration set that contains the event destination that you want to update.
--
-- * 'ucsedEventDestination' - The event destination object that you want to apply to the specified configuration set.
updateConfigurationSetEventDestination
    :: Text -- ^ 'ucsedConfigurationSetName'
    -> EventDestination -- ^ 'ucsedEventDestination'
    -> UpdateConfigurationSetEventDestination
updateConfigurationSetEventDestination pConfigurationSetName_ pEventDestination_ =
  UpdateConfigurationSetEventDestination'
    { _ucsedConfigurationSetName = pConfigurationSetName_
    , _ucsedEventDestination = pEventDestination_
    }


-- | The name of the configuration set that contains the event destination that you want to update.
ucsedConfigurationSetName :: Lens' UpdateConfigurationSetEventDestination Text
ucsedConfigurationSetName = lens _ucsedConfigurationSetName (\ s a -> s{_ucsedConfigurationSetName = a})

-- | The event destination object that you want to apply to the specified configuration set.
ucsedEventDestination :: Lens' UpdateConfigurationSetEventDestination EventDestination
ucsedEventDestination = lens _ucsedEventDestination (\ s a -> s{_ucsedEventDestination = a})

instance AWSRequest
           UpdateConfigurationSetEventDestination
         where
        type Rs UpdateConfigurationSetEventDestination =
             UpdateConfigurationSetEventDestinationResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "UpdateConfigurationSetEventDestinationResult"
              (\ s h x ->
                 UpdateConfigurationSetEventDestinationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           UpdateConfigurationSetEventDestination
         where

instance NFData
           UpdateConfigurationSetEventDestination
         where

instance ToHeaders
           UpdateConfigurationSetEventDestination
         where
        toHeaders = const mempty

instance ToPath
           UpdateConfigurationSetEventDestination
         where
        toPath = const "/"

instance ToQuery
           UpdateConfigurationSetEventDestination
         where
        toQuery UpdateConfigurationSetEventDestination'{..}
          = mconcat
              ["Action" =:
                 ("UpdateConfigurationSetEventDestination" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _ucsedConfigurationSetName,
               "EventDestination" =: _ucsedEventDestination]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'updateConfigurationSetEventDestinationResponse' smart constructor.
newtype UpdateConfigurationSetEventDestinationResponse = UpdateConfigurationSetEventDestinationResponse'
  { _ucsedrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetEventDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsedrsResponseStatus' - -- | The response status code.
updateConfigurationSetEventDestinationResponse
    :: Int -- ^ 'ucsedrsResponseStatus'
    -> UpdateConfigurationSetEventDestinationResponse
updateConfigurationSetEventDestinationResponse pResponseStatus_ =
  UpdateConfigurationSetEventDestinationResponse'
    {_ucsedrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucsedrsResponseStatus :: Lens' UpdateConfigurationSetEventDestinationResponse Int
ucsedrsResponseStatus = lens _ucsedrsResponseStatus (\ s a -> s{_ucsedrsResponseStatus = a})

instance NFData
           UpdateConfigurationSetEventDestinationResponse
         where
