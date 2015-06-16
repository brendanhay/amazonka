{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation updates the type of medium changer in a gateway-VTL. When
-- you activate a gateway-VTL, you select a medium changer type for the
-- gateway-VTL. This operation enables you to select a different type of
-- medium changer after a gateway-VTL is activated.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateVTLDeviceType.html>
module Network.AWS.StorageGateway.UpdateVTLDeviceType
    (
    -- * Request
      UpdateVTLDeviceType
    -- ** Request constructor
    , updateVTLDeviceType
    -- ** Request lenses
    , uvtldtVTLDeviceARN
    , uvtldtDeviceType

    -- * Response
    , UpdateVTLDeviceTypeResponse
    -- ** Response constructor
    , updateVTLDeviceTypeResponse
    -- ** Response lenses
    , uvtldtrVTLDeviceARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'updateVTLDeviceType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvtldtVTLDeviceARN'
--
-- * 'uvtldtDeviceType'
data UpdateVTLDeviceType = UpdateVTLDeviceType'{_uvtldtVTLDeviceARN :: Text, _uvtldtDeviceType :: Text} deriving (Eq, Read, Show)

-- | 'UpdateVTLDeviceType' smart constructor.
updateVTLDeviceType :: Text -> Text -> UpdateVTLDeviceType
updateVTLDeviceType pVTLDeviceARN pDeviceType = UpdateVTLDeviceType'{_uvtldtVTLDeviceARN = pVTLDeviceARN, _uvtldtDeviceType = pDeviceType};

-- | The Amazon Resource Name (ARN) of the medium changer you want to select.
uvtldtVTLDeviceARN :: Lens' UpdateVTLDeviceType Text
uvtldtVTLDeviceARN = lens _uvtldtVTLDeviceARN (\ s a -> s{_uvtldtVTLDeviceARN = a});

-- | The type of medium changer you want to select.
--
-- /Valid Values/: \"STK-L700\", \"AWS-Gateway-VTL\"
uvtldtDeviceType :: Lens' UpdateVTLDeviceType Text
uvtldtDeviceType = lens _uvtldtDeviceType (\ s a -> s{_uvtldtDeviceType = a});

instance AWSRequest UpdateVTLDeviceType where
        type Sv UpdateVTLDeviceType = StorageGateway
        type Rs UpdateVTLDeviceType =
             UpdateVTLDeviceTypeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateVTLDeviceTypeResponse' <$>
                   (x .?> "VTLDeviceARN"))

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
              ["VTLDeviceARN" .= _uvtldtVTLDeviceARN,
               "DeviceType" .= _uvtldtDeviceType]

instance ToPath UpdateVTLDeviceType where
        toPath = const "/"

instance ToQuery UpdateVTLDeviceType where
        toQuery = const mempty

-- | /See:/ 'updateVTLDeviceTypeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvtldtrVTLDeviceARN'
newtype UpdateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse'{_uvtldtrVTLDeviceARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UpdateVTLDeviceTypeResponse' smart constructor.
updateVTLDeviceTypeResponse :: UpdateVTLDeviceTypeResponse
updateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse'{_uvtldtrVTLDeviceARN = Nothing};

-- | The Amazon Resource Name (ARN) of the medium changer you have selected.
uvtldtrVTLDeviceARN :: Lens' UpdateVTLDeviceTypeResponse (Maybe Text)
uvtldtrVTLDeviceARN = lens _uvtldtrVTLDeviceARN (\ s a -> s{_uvtldtrVTLDeviceARN = a});
