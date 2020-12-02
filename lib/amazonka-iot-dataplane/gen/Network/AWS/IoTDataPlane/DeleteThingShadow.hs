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
-- Module      : Network.AWS.IoTDataPlane.DeleteThingShadow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the thing shadow for the specified thing.
--
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html DeleteThingShadow> in the /AWS IoT Developer Guide/ .
--
module Network.AWS.IoTDataPlane.DeleteThingShadow
    (
    -- * Creating a Request
      deleteThingShadow
    , DeleteThingShadow
    -- * Request Lenses
    , dtsThingName

    -- * Destructuring the Response
    , deleteThingShadowResponse
    , DeleteThingShadowResponse
    -- * Response Lenses
    , dtsrsResponseStatus
    , dtsrsPayload
    ) where

import Network.AWS.IoTDataPlane.Types
import Network.AWS.IoTDataPlane.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DeleteThingShadow operation.
--
--
--
-- /See:/ 'deleteThingShadow' smart constructor.
newtype DeleteThingShadow = DeleteThingShadow'
  { _dtsThingName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingShadow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsThingName' - The name of the thing.
deleteThingShadow
    :: Text -- ^ 'dtsThingName'
    -> DeleteThingShadow
deleteThingShadow pThingName_ = DeleteThingShadow' {_dtsThingName = pThingName_}


-- | The name of the thing.
dtsThingName :: Lens' DeleteThingShadow Text
dtsThingName = lens _dtsThingName (\ s a -> s{_dtsThingName = a});

instance AWSRequest DeleteThingShadow where
        type Rs DeleteThingShadow = DeleteThingShadowResponse
        request = delete ioTDataPlane
        response
          = receiveJSON
              (\ s h x ->
                 DeleteThingShadowResponse' <$>
                   (pure (fromEnum s)) <*> (pure x))

instance Hashable DeleteThingShadow where

instance NFData DeleteThingShadow where

instance ToHeaders DeleteThingShadow where
        toHeaders = const mempty

instance ToPath DeleteThingShadow where
        toPath DeleteThingShadow'{..}
          = mconcat ["/things/", toBS _dtsThingName, "/shadow"]

instance ToQuery DeleteThingShadow where
        toQuery = const mempty

-- | The output from the DeleteThingShadow operation.
--
--
--
-- /See:/ 'deleteThingShadowResponse' smart constructor.
data DeleteThingShadowResponse = DeleteThingShadowResponse'
  { _dtsrsResponseStatus :: !Int
  , _dtsrsPayload        :: !(HashMap Text Value)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingShadowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsrsResponseStatus' - -- | The response status code.
--
-- * 'dtsrsPayload' - The state information, in JSON format.
deleteThingShadowResponse
    :: Int -- ^ 'dtsrsResponseStatus'
    -> HashMap Text Value -- ^ 'dtsrsPayload'
    -> DeleteThingShadowResponse
deleteThingShadowResponse pResponseStatus_ pPayload_ =
  DeleteThingShadowResponse'
  {_dtsrsResponseStatus = pResponseStatus_, _dtsrsPayload = pPayload_}


-- | -- | The response status code.
dtsrsResponseStatus :: Lens' DeleteThingShadowResponse Int
dtsrsResponseStatus = lens _dtsrsResponseStatus (\ s a -> s{_dtsrsResponseStatus = a});

-- | The state information, in JSON format.
dtsrsPayload :: Lens' DeleteThingShadowResponse (HashMap Text Value)
dtsrsPayload = lens _dtsrsPayload (\ s a -> s{_dtsrsPayload = a});

instance NFData DeleteThingShadowResponse where
