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
-- Module      : Network.AWS.IoTData.Publish
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes state information.
--
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http HTTP Protocol> in the /AWS IoT Developer Guide/ .
--
module Network.AWS.IoTData.Publish
    (
    -- * Creating a Request
      publish
    , Publish
    -- * Request Lenses
    , pPayload
    , pQos
    , pTopic

    -- * Destructuring the Response
    , publishResponse
    , PublishResponse
    ) where

import Network.AWS.IoTData.Types
import Network.AWS.IoTData.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the Publish operation.
--
--
--
-- /See:/ 'publish' smart constructor.
data Publish = Publish'
  { _pPayload :: !(Maybe ByteString)
  , _pQos     :: !(Maybe Nat)
  , _pTopic   :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Publish' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPayload' - The state information, in JSON format.
--
-- * 'pQos' - The Quality of Service (QoS) level.
--
-- * 'pTopic' - The name of the MQTT topic.
publish
    :: Text -- ^ 'pTopic'
    -> Publish
publish pTopic_ =
  Publish' {_pPayload = Nothing, _pQos = Nothing, _pTopic = pTopic_}


-- | The state information, in JSON format.
pPayload :: Lens' Publish (Maybe ByteString)
pPayload = lens _pPayload (\ s a -> s{_pPayload = a})

-- | The Quality of Service (QoS) level.
pQos :: Lens' Publish (Maybe Natural)
pQos = lens _pQos (\ s a -> s{_pQos = a}) . mapping _Nat

-- | The name of the MQTT topic.
pTopic :: Lens' Publish Text
pTopic = lens _pTopic (\ s a -> s{_pTopic = a})

instance AWSRequest Publish where
        type Rs Publish = PublishResponse
        request = postBody ioTData
        response = receiveNull PublishResponse'

instance Hashable Publish where

instance NFData Publish where

instance ToBody Publish where
        toBody = toBody . _pPayload

instance ToHeaders Publish where
        toHeaders = const mempty

instance ToPath Publish where
        toPath Publish'{..}
          = mconcat ["/topics/", toBS _pTopic]

instance ToQuery Publish where
        toQuery Publish'{..} = mconcat ["qos" =: _pQos]

-- | /See:/ 'publishResponse' smart constructor.
data PublishResponse =
  PublishResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublishResponse' with the minimum fields required to make a request.
--
publishResponse
    :: PublishResponse
publishResponse = PublishResponse'


instance NFData PublishResponse where
