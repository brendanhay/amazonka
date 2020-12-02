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
-- Module      : Network.AWS.SES.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.DeleteConfigurationSetEventDestination
    (
    -- * Creating a Request
      deleteConfigurationSetEventDestination
    , DeleteConfigurationSetEventDestination
    -- * Request Lenses
    , dcsedConfigurationSetName
    , dcsedEventDestinationName

    -- * Destructuring the Response
    , deleteConfigurationSetEventDestinationResponse
    , DeleteConfigurationSetEventDestinationResponse
    -- * Response Lenses
    , dcsedrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to delete a configuration set event destination. Configuration set event destinations are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'deleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { _dcsedConfigurationSetName :: !Text
  , _dcsedEventDestinationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationSetEventDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsedConfigurationSetName' - The name of the configuration set from which to delete the event destination.
--
-- * 'dcsedEventDestinationName' - The name of the event destination to delete.
deleteConfigurationSetEventDestination
    :: Text -- ^ 'dcsedConfigurationSetName'
    -> Text -- ^ 'dcsedEventDestinationName'
    -> DeleteConfigurationSetEventDestination
deleteConfigurationSetEventDestination pConfigurationSetName_ pEventDestinationName_ =
  DeleteConfigurationSetEventDestination'
    { _dcsedConfigurationSetName = pConfigurationSetName_
    , _dcsedEventDestinationName = pEventDestinationName_
    }


-- | The name of the configuration set from which to delete the event destination.
dcsedConfigurationSetName :: Lens' DeleteConfigurationSetEventDestination Text
dcsedConfigurationSetName = lens _dcsedConfigurationSetName (\ s a -> s{_dcsedConfigurationSetName = a})

-- | The name of the event destination to delete.
dcsedEventDestinationName :: Lens' DeleteConfigurationSetEventDestination Text
dcsedEventDestinationName = lens _dcsedEventDestinationName (\ s a -> s{_dcsedEventDestinationName = a})

instance AWSRequest
           DeleteConfigurationSetEventDestination
         where
        type Rs DeleteConfigurationSetEventDestination =
             DeleteConfigurationSetEventDestinationResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "DeleteConfigurationSetEventDestinationResult"
              (\ s h x ->
                 DeleteConfigurationSetEventDestinationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable
           DeleteConfigurationSetEventDestination
         where

instance NFData
           DeleteConfigurationSetEventDestination
         where

instance ToHeaders
           DeleteConfigurationSetEventDestination
         where
        toHeaders = const mempty

instance ToPath
           DeleteConfigurationSetEventDestination
         where
        toPath = const "/"

instance ToQuery
           DeleteConfigurationSetEventDestination
         where
        toQuery DeleteConfigurationSetEventDestination'{..}
          = mconcat
              ["Action" =:
                 ("DeleteConfigurationSetEventDestination" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _dcsedConfigurationSetName,
               "EventDestinationName" =: _dcsedEventDestinationName]

-- | An empty element returned on a successful request.
--
--
--
-- /See:/ 'deleteConfigurationSetEventDestinationResponse' smart constructor.
newtype DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { _dcsedrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigurationSetEventDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsedrsResponseStatus' - -- | The response status code.
deleteConfigurationSetEventDestinationResponse
    :: Int -- ^ 'dcsedrsResponseStatus'
    -> DeleteConfigurationSetEventDestinationResponse
deleteConfigurationSetEventDestinationResponse pResponseStatus_ =
  DeleteConfigurationSetEventDestinationResponse'
    {_dcsedrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcsedrsResponseStatus :: Lens' DeleteConfigurationSetEventDestinationResponse Int
dcsedrsResponseStatus = lens _dcsedrsResponseStatus (\ s a -> s{_dcsedrsResponseStatus = a})

instance NFData
           DeleteConfigurationSetEventDestinationResponse
         where
