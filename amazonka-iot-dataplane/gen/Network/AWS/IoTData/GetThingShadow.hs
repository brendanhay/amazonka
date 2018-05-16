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
-- Module      : Network.AWS.IoTData.GetThingShadow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the thing shadow for the specified thing.
--
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html GetThingShadow> in the /AWS IoT Developer Guide/ .
--
module Network.AWS.IoTData.GetThingShadow
    (
    -- * Creating a Request
      getThingShadow
    , GetThingShadow
    -- * Request Lenses
    , gtsThingName

    -- * Destructuring the Response
    , getThingShadowResponse
    , GetThingShadowResponse
    -- * Response Lenses
    , gtsrsPayload
    , gtsrsResponseStatus
    ) where

import Network.AWS.IoTData.Types
import Network.AWS.IoTData.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the GetThingShadow operation.
--
--
--
-- /See:/ 'getThingShadow' smart constructor.
newtype GetThingShadow = GetThingShadow'
  { _gtsThingName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetThingShadow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsThingName' - The name of the thing.
getThingShadow
    :: Text -- ^ 'gtsThingName'
    -> GetThingShadow
getThingShadow pThingName_ = GetThingShadow' {_gtsThingName = pThingName_}


-- | The name of the thing.
gtsThingName :: Lens' GetThingShadow Text
gtsThingName = lens _gtsThingName (\ s a -> s{_gtsThingName = a})

instance AWSRequest GetThingShadow where
        type Rs GetThingShadow = GetThingShadowResponse
        request = get ioTData
        response
          = receiveBytes
              (\ s h x ->
                 GetThingShadowResponse' <$>
                   (pure (Just x)) <*> (pure (fromEnum s)))

instance Hashable GetThingShadow where

instance NFData GetThingShadow where

instance ToHeaders GetThingShadow where
        toHeaders = const mempty

instance ToPath GetThingShadow where
        toPath GetThingShadow'{..}
          = mconcat ["/things/", toBS _gtsThingName, "/shadow"]

instance ToQuery GetThingShadow where
        toQuery = const mempty

-- | The output from the GetThingShadow operation.
--
--
--
-- /See:/ 'getThingShadowResponse' smart constructor.
data GetThingShadowResponse = GetThingShadowResponse'
  { _gtsrsPayload        :: !(Maybe ByteString)
  , _gtsrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetThingShadowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsrsPayload' - The state information, in JSON format.
--
-- * 'gtsrsResponseStatus' - -- | The response status code.
getThingShadowResponse
    :: Int -- ^ 'gtsrsResponseStatus'
    -> GetThingShadowResponse
getThingShadowResponse pResponseStatus_ =
  GetThingShadowResponse'
    {_gtsrsPayload = Nothing, _gtsrsResponseStatus = pResponseStatus_}


-- | The state information, in JSON format.
gtsrsPayload :: Lens' GetThingShadowResponse (Maybe ByteString)
gtsrsPayload = lens _gtsrsPayload (\ s a -> s{_gtsrsPayload = a})

-- | -- | The response status code.
gtsrsResponseStatus :: Lens' GetThingShadowResponse Int
gtsrsResponseStatus = lens _gtsrsResponseStatus (\ s a -> s{_gtsrsResponseStatus = a})

instance NFData GetThingShadowResponse where
