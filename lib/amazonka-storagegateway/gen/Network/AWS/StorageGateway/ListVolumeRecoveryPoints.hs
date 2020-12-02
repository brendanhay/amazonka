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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recovery points for a specified gateway. This operation is only supported in the cached volume gateway type.
--
--
-- Each cache volume has one recovery point. A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot or clone a new cached volume from a source volume. To create a snapshot from a volume recovery point use the 'CreateSnapshotFromVolumeRecoveryPoint' operation.
--
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
    , lvrprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'listVolumeRecoveryPoints' smart constructor.
newtype ListVolumeRecoveryPoints = ListVolumeRecoveryPoints'
  { _lvrpGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVolumeRecoveryPoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrpGatewayARN' - Undocumented member.
listVolumeRecoveryPoints
    :: Text -- ^ 'lvrpGatewayARN'
    -> ListVolumeRecoveryPoints
listVolumeRecoveryPoints pGatewayARN_ =
  ListVolumeRecoveryPoints' {_lvrpGatewayARN = pGatewayARN_}


-- | Undocumented member.
lvrpGatewayARN :: Lens' ListVolumeRecoveryPoints Text
lvrpGatewayARN = lens _lvrpGatewayARN (\ s a -> s{_lvrpGatewayARN = a})

instance AWSRequest ListVolumeRecoveryPoints where
        type Rs ListVolumeRecoveryPoints =
             ListVolumeRecoveryPointsResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumeRecoveryPointsResponse' <$>
                   (x .?> "VolumeRecoveryPointInfos" .!@ mempty) <*>
                     (x .?> "GatewayARN")
                     <*> (pure (fromEnum s)))

instance Hashable ListVolumeRecoveryPoints where

instance NFData ListVolumeRecoveryPoints where

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
          = object
              (catMaybes [Just ("GatewayARN" .= _lvrpGatewayARN)])

instance ToPath ListVolumeRecoveryPoints where
        toPath = const "/"

instance ToQuery ListVolumeRecoveryPoints where
        toQuery = const mempty

-- | /See:/ 'listVolumeRecoveryPointsResponse' smart constructor.
data ListVolumeRecoveryPointsResponse = ListVolumeRecoveryPointsResponse'
  { _lvrprsVolumeRecoveryPointInfos :: !(Maybe [VolumeRecoveryPointInfo])
  , _lvrprsGatewayARN               :: !(Maybe Text)
  , _lvrprsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVolumeRecoveryPointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrprsVolumeRecoveryPointInfos' - Undocumented member.
--
-- * 'lvrprsGatewayARN' - Undocumented member.
--
-- * 'lvrprsResponseStatus' - -- | The response status code.
listVolumeRecoveryPointsResponse
    :: Int -- ^ 'lvrprsResponseStatus'
    -> ListVolumeRecoveryPointsResponse
listVolumeRecoveryPointsResponse pResponseStatus_ =
  ListVolumeRecoveryPointsResponse'
    { _lvrprsVolumeRecoveryPointInfos = Nothing
    , _lvrprsGatewayARN = Nothing
    , _lvrprsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lvrprsVolumeRecoveryPointInfos :: Lens' ListVolumeRecoveryPointsResponse [VolumeRecoveryPointInfo]
lvrprsVolumeRecoveryPointInfos = lens _lvrprsVolumeRecoveryPointInfos (\ s a -> s{_lvrprsVolumeRecoveryPointInfos = a}) . _Default . _Coerce

-- | Undocumented member.
lvrprsGatewayARN :: Lens' ListVolumeRecoveryPointsResponse (Maybe Text)
lvrprsGatewayARN = lens _lvrprsGatewayARN (\ s a -> s{_lvrprsGatewayARN = a})

-- | -- | The response status code.
lvrprsResponseStatus :: Lens' ListVolumeRecoveryPointsResponse Int
lvrprsResponseStatus = lens _lvrprsResponseStatus (\ s a -> s{_lvrprsResponseStatus = a})

instance NFData ListVolumeRecoveryPointsResponse
         where
