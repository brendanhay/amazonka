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
-- Module      : Network.AWS.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the type of medium changer in a tape gateway. When you activate a tape gateway, you select a medium changer type for the tape gateway. This operation enables you to select a different type of medium changer after a tape gateway is activated. This operation is only supported in the tape gateway type.
--
--
module Network.AWS.StorageGateway.UpdateVTLDeviceType
    (
    -- * Creating a Request
      updateVTLDeviceType
    , UpdateVTLDeviceType
    -- * Request Lenses
    , uvtldtVTLDeviceARN
    , uvtldtDeviceType

    -- * Destructuring the Response
    , updateVTLDeviceTypeResponse
    , UpdateVTLDeviceTypeResponse
    -- * Response Lenses
    , uvtldtrsVTLDeviceARN
    , uvtldtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'updateVTLDeviceType' smart constructor.
data UpdateVTLDeviceType = UpdateVTLDeviceType'
  { _uvtldtVTLDeviceARN :: !Text
  , _uvtldtDeviceType   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVTLDeviceType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvtldtVTLDeviceARN' - The Amazon Resource Name (ARN) of the medium changer you want to select.
--
-- * 'uvtldtDeviceType' - The type of medium changer you want to select. Valid Values: "STK-L700", "AWS-Gateway-VTL"
updateVTLDeviceType
    :: Text -- ^ 'uvtldtVTLDeviceARN'
    -> Text -- ^ 'uvtldtDeviceType'
    -> UpdateVTLDeviceType
updateVTLDeviceType pVTLDeviceARN_ pDeviceType_ =
  UpdateVTLDeviceType'
    {_uvtldtVTLDeviceARN = pVTLDeviceARN_, _uvtldtDeviceType = pDeviceType_}


-- | The Amazon Resource Name (ARN) of the medium changer you want to select.
uvtldtVTLDeviceARN :: Lens' UpdateVTLDeviceType Text
uvtldtVTLDeviceARN = lens _uvtldtVTLDeviceARN (\ s a -> s{_uvtldtVTLDeviceARN = a})

-- | The type of medium changer you want to select. Valid Values: "STK-L700", "AWS-Gateway-VTL"
uvtldtDeviceType :: Lens' UpdateVTLDeviceType Text
uvtldtDeviceType = lens _uvtldtDeviceType (\ s a -> s{_uvtldtDeviceType = a})

instance AWSRequest UpdateVTLDeviceType where
        type Rs UpdateVTLDeviceType =
             UpdateVTLDeviceTypeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVTLDeviceTypeResponse' <$>
                   (x .?> "VTLDeviceARN") <*> (pure (fromEnum s)))

instance Hashable UpdateVTLDeviceType where

instance NFData UpdateVTLDeviceType where

instance ToHeaders UpdateVTLDeviceType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateVTLDeviceType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVTLDeviceType where
        toJSON UpdateVTLDeviceType'{..}
          = object
              (catMaybes
                 [Just ("VTLDeviceARN" .= _uvtldtVTLDeviceARN),
                  Just ("DeviceType" .= _uvtldtDeviceType)])

instance ToPath UpdateVTLDeviceType where
        toPath = const "/"

instance ToQuery UpdateVTLDeviceType where
        toQuery = const mempty

-- | UpdateVTLDeviceTypeOutput
--
--
--
-- /See:/ 'updateVTLDeviceTypeResponse' smart constructor.
data UpdateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse'
  { _uvtldtrsVTLDeviceARN   :: !(Maybe Text)
  , _uvtldtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVTLDeviceTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvtldtrsVTLDeviceARN' - The Amazon Resource Name (ARN) of the medium changer you have selected.
--
-- * 'uvtldtrsResponseStatus' - -- | The response status code.
updateVTLDeviceTypeResponse
    :: Int -- ^ 'uvtldtrsResponseStatus'
    -> UpdateVTLDeviceTypeResponse
updateVTLDeviceTypeResponse pResponseStatus_ =
  UpdateVTLDeviceTypeResponse'
    { _uvtldtrsVTLDeviceARN = Nothing
    , _uvtldtrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the medium changer you have selected.
uvtldtrsVTLDeviceARN :: Lens' UpdateVTLDeviceTypeResponse (Maybe Text)
uvtldtrsVTLDeviceARN = lens _uvtldtrsVTLDeviceARN (\ s a -> s{_uvtldtrsVTLDeviceARN = a})

-- | -- | The response status code.
uvtldtrsResponseStatus :: Lens' UpdateVTLDeviceTypeResponse Int
uvtldtrsResponseStatus = lens _uvtldtrsResponseStatus (\ s a -> s{_uvtldtrsResponseStatus = a})

instance NFData UpdateVTLDeviceTypeResponse where
