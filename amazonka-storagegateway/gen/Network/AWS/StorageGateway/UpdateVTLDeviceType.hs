{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateVTLDeviceType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the type of medium changer in a gateway-VTL. When
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
    , uvtldtrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | UpdateVTLDeviceTypeInput
--
-- /See:/ 'updateVTLDeviceType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvtldtVTLDeviceARN'
--
-- * 'uvtldtDeviceType'
data UpdateVTLDeviceType = UpdateVTLDeviceType'
    { _uvtldtVTLDeviceARN :: !Text
    , _uvtldtDeviceType   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateVTLDeviceType' smart constructor.
updateVTLDeviceType :: Text -> Text -> UpdateVTLDeviceType
updateVTLDeviceType pVTLDeviceARN pDeviceType =
    UpdateVTLDeviceType'
    { _uvtldtVTLDeviceARN = pVTLDeviceARN
    , _uvtldtDeviceType = pDeviceType
    }

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
                   (x .?> "VTLDeviceARN") <*> (pure (fromEnum s)))

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

-- | UpdateVTLDeviceTypeOutput
--
-- /See:/ 'updateVTLDeviceTypeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uvtldtrVTLDeviceARN'
--
-- * 'uvtldtrStatus'
data UpdateVTLDeviceTypeResponse = UpdateVTLDeviceTypeResponse'
    { _uvtldtrVTLDeviceARN :: !(Maybe Text)
    , _uvtldtrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateVTLDeviceTypeResponse' smart constructor.
updateVTLDeviceTypeResponse :: Int -> UpdateVTLDeviceTypeResponse
updateVTLDeviceTypeResponse pStatus =
    UpdateVTLDeviceTypeResponse'
    { _uvtldtrVTLDeviceARN = Nothing
    , _uvtldtrStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the medium changer you have selected.
uvtldtrVTLDeviceARN :: Lens' UpdateVTLDeviceTypeResponse (Maybe Text)
uvtldtrVTLDeviceARN = lens _uvtldtrVTLDeviceARN (\ s a -> s{_uvtldtrVTLDeviceARN = a});

-- | FIXME: Undocumented member.
uvtldtrStatus :: Lens' UpdateVTLDeviceTypeResponse Int
uvtldtrStatus = lens _uvtldtrStatus (\ s a -> s{_uvtldtrStatus = a});
