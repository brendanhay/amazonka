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
-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the endpoint for a device and mobile app from Amazon SNS. This action is idempotent. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
-- When you delete an endpoint that is also subscribed to a topic, then you must also unsubscribe the endpoint from the topic.
--
module Network.AWS.SNS.DeleteEndpoint
    (
    -- * Creating a Request
      deleteEndpoint
    , DeleteEndpoint
    -- * Request Lenses
    , deEndpointARN

    -- * Destructuring the Response
    , deleteEndpointResponse
    , DeleteEndpointResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for DeleteEndpoint action.
--
--
--
-- /See:/ 'deleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint'
  { _deEndpointARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEndpointARN' - EndpointArn of endpoint to delete.
deleteEndpoint
    :: Text -- ^ 'deEndpointARN'
    -> DeleteEndpoint
deleteEndpoint pEndpointARN_ = DeleteEndpoint' {_deEndpointARN = pEndpointARN_}


-- | EndpointArn of endpoint to delete.
deEndpointARN :: Lens' DeleteEndpoint Text
deEndpointARN = lens _deEndpointARN (\ s a -> s{_deEndpointARN = a})

instance AWSRequest DeleteEndpoint where
        type Rs DeleteEndpoint = DeleteEndpointResponse
        request = postQuery sns
        response = receiveNull DeleteEndpointResponse'

instance Hashable DeleteEndpoint where

instance NFData DeleteEndpoint where

instance ToHeaders DeleteEndpoint where
        toHeaders = const mempty

instance ToPath DeleteEndpoint where
        toPath = const "/"

instance ToQuery DeleteEndpoint where
        toQuery DeleteEndpoint'{..}
          = mconcat
              ["Action" =: ("DeleteEndpoint" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "EndpointArn" =: _deEndpointARN]

-- | /See:/ 'deleteEndpointResponse' smart constructor.
data DeleteEndpointResponse =
  DeleteEndpointResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
--
deleteEndpointResponse
    :: DeleteEndpointResponse
deleteEndpointResponse = DeleteEndpointResponse'


instance NFData DeleteEndpointResponse where
