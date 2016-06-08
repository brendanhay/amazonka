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
-- Module      : Network.AWS.IoTDataPlane.UpdateThingShadow
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the thing shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html UpdateThingShadow> in the /AWS IoT Developer Guide/.
module Network.AWS.IoTDataPlane.UpdateThingShadow
    (
    -- * Creating a Request
      updateThingShadow
    , UpdateThingShadow
    -- * Request Lenses
    , utsThingName
    , utsPayload

    -- * Destructuring the Response
    , updateThingShadowResponse
    , UpdateThingShadowResponse
    -- * Response Lenses
    , utsrsPayload
    , utsrsResponseStatus
    ) where

import           Network.AWS.IoTDataPlane.Types
import           Network.AWS.IoTDataPlane.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the UpdateThingShadow operation.
--
-- /See:/ 'updateThingShadow' smart constructor.
data UpdateThingShadow = UpdateThingShadow'
    { _utsThingName :: !Text
    , _utsPayload   :: !(HashMap Text Value)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateThingShadow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utsThingName'
--
-- * 'utsPayload'
updateThingShadow
    :: Text -- ^ 'utsThingName'
    -> HashMap Text Value -- ^ 'utsPayload'
    -> UpdateThingShadow
updateThingShadow pThingName_ pPayload_ =
    UpdateThingShadow'
    { _utsThingName = pThingName_
    , _utsPayload = pPayload_
    }

-- | The name of the thing.
utsThingName :: Lens' UpdateThingShadow Text
utsThingName = lens _utsThingName (\ s a -> s{_utsThingName = a});

-- | The state information, in JSON format.
utsPayload :: Lens' UpdateThingShadow (HashMap Text Value)
utsPayload = lens _utsPayload (\ s a -> s{_utsPayload = a});

instance AWSRequest UpdateThingShadow where
        type Rs UpdateThingShadow = UpdateThingShadowResponse
        request = postBody ioTDataPlane
        response
          = receiveJSON
              (\ s h x ->
                 UpdateThingShadowResponse' <$>
                   (pure (Just x)) <*> (pure (fromEnum s)))

instance Hashable UpdateThingShadow

instance NFData UpdateThingShadow

instance ToBody UpdateThingShadow where
        toBody = toBody . _utsPayload

instance ToHeaders UpdateThingShadow where
        toHeaders = const mempty

instance ToPath UpdateThingShadow where
        toPath UpdateThingShadow'{..}
          = mconcat ["/things/", toBS _utsThingName, "/shadow"]

instance ToQuery UpdateThingShadow where
        toQuery = const mempty

-- | The output from the UpdateThingShadow operation.
--
-- /See:/ 'updateThingShadowResponse' smart constructor.
data UpdateThingShadowResponse = UpdateThingShadowResponse'
    { _utsrsPayload        :: !(Maybe (HashMap Text Value))
    , _utsrsResponseStatus :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateThingShadowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utsrsPayload'
--
-- * 'utsrsResponseStatus'
updateThingShadowResponse
    :: Int -- ^ 'utsrsResponseStatus'
    -> UpdateThingShadowResponse
updateThingShadowResponse pResponseStatus_ =
    UpdateThingShadowResponse'
    { _utsrsPayload = Nothing
    , _utsrsResponseStatus = pResponseStatus_
    }

-- | The state information, in JSON format.
utsrsPayload :: Lens' UpdateThingShadowResponse (Maybe (HashMap Text Value))
utsrsPayload = lens _utsrsPayload (\ s a -> s{_utsrsPayload = a});

-- | The response status code.
utsrsResponseStatus :: Lens' UpdateThingShadowResponse Int
utsrsResponseStatus = lens _utsrsResponseStatus (\ s a -> s{_utsrsResponseStatus = a});

instance NFData UpdateThingShadowResponse
