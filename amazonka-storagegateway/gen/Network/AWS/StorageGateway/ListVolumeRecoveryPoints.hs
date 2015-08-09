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
-- Module      : Network.AWS.StorageGateway.ListVolumeRecoveryPoints
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the recovery points for a specified gateway. This
-- operation is supported only for the gateway-cached volume architecture.
--
-- Each gateway-cached volume has one recovery point. A volume recovery
-- point is a point in time at which all data of the volume is consistent
-- and from which you can create a snapshot. To create a snapshot from a
-- volume recovery point use the CreateSnapshotFromVolumeRecoveryPoint
-- operation.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListVolumeRecoveryPoints.html AWS API Reference> for ListVolumeRecoveryPoints.
module Network.AWS.StorageGateway.ListVolumeRecoveryPoints
    (
    -- * Creating a Request
      listVolumeRecoveryPoints
    , ListVolumeRecoveryPoints
    -- * Request Lenses
    , lvrpGatewayARN

    -- * Destructuring the Response
    , listVolumeRecoveryPointsResponse
    , ListVolumeRecoveryPointsResponse
    -- * Response Lenses
    , lvrprsVolumeRecoveryPointInfos
    , lvrprsGatewayARN
    , lvrprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'listVolumeRecoveryPoints' smart constructor.
newtype ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
    { _lvrpGatewayARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListVolumeRecoveryPoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrpGatewayARN'
listVolumeRecoveryPoints
    :: Text -- ^ 'lvrpGatewayARN'
    -> ListVolumeRecoveryPoints
listVolumeRecoveryPoints pGatewayARN_ =
    ListVolumeRecoveryPoints'
    { _lvrpGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
lvrpGatewayARN :: Lens' ListVolumeRecoveryPoints Text
lvrpGatewayARN = lens _lvrpGatewayARN (\ s a -> s{_lvrpGatewayARN = a});

instance AWSRequest ListVolumeRecoveryPoints where
        type Sv ListVolumeRecoveryPoints = StorageGateway
        type Rs ListVolumeRecoveryPoints =
             ListVolumeRecoveryPointsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumeRecoveryPointsResponse' <$>
                   (x .?> "VolumeRecoveryPointInfos" .!@ mempty) <*>
                     (x .?> "GatewayARN")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListVolumeRecoveryPoints where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListVolumeRecoveryPoints"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListVolumeRecoveryPoints where
        toJSON ListVolumeRecoveryPoints'{..}
          = object ["GatewayARN" .= _lvrpGatewayARN]

instance ToPath ListVolumeRecoveryPoints where
        toPath = const "/"

instance ToQuery ListVolumeRecoveryPoints where
        toQuery = const mempty

-- | /See:/ 'listVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
    { _lvrprsVolumeRecoveryPointInfos :: !(Maybe [VolumeRecoveryPointInfo])
    , _lvrprsGatewayARN               :: !(Maybe Text)
    , _lvrprsStatus                   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListVolumeRecoveryPointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrprsVolumeRecoveryPointInfos'
--
-- * 'lvrprsGatewayARN'
--
-- * 'lvrprsStatus'
listVolumeRecoveryPointsResponse
    :: Int -- ^ 'lvrprsStatus'
    -> ListVolumeRecoveryPointsResponse
listVolumeRecoveryPointsResponse pStatus_ =
    ListVolumeRecoveryPointsResponse'
    { _lvrprsVolumeRecoveryPointInfos = Nothing
    , _lvrprsGatewayARN = Nothing
    , _lvrprsStatus = pStatus_
    }

-- | Undocumented member.
lvrprsVolumeRecoveryPointInfos :: Lens' ListVolumeRecoveryPointsResponse [VolumeRecoveryPointInfo]
lvrprsVolumeRecoveryPointInfos = lens _lvrprsVolumeRecoveryPointInfos (\ s a -> s{_lvrprsVolumeRecoveryPointInfos = a}) . _Default . _Coerce;

-- | Undocumented member.
lvrprsGatewayARN :: Lens' ListVolumeRecoveryPointsResponse (Maybe Text)
lvrprsGatewayARN = lens _lvrprsGatewayARN (\ s a -> s{_lvrprsGatewayARN = a});

-- | The response status code.
lvrprsStatus :: Lens' ListVolumeRecoveryPointsResponse Int
lvrprsStatus = lens _lvrprsStatus (\ s a -> s{_lvrprsStatus = a});
