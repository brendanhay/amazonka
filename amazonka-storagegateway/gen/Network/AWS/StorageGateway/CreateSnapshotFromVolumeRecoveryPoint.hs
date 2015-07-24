{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a snapshot of a gateway from a volume recovery
-- point. This operation is supported only for the gateway-cached volume
-- architecture (see ).
--
-- A volume recovery point is a point in time at which all data of the
-- volume is consistent and from which you can create a snapshot. To get a
-- list of volume recovery point for gateway-cached volumes, use
-- ListVolumeRecoveryPoints.
--
-- In the @CreateSnapshotFromVolumeRecoveryPoint@ request, you identify the
-- volume by providing its Amazon Resource Name (ARN). You must also
-- provide a description for the snapshot. When AWS Storage Gateway takes a
-- snapshot of the specified volume, the snapshot and its description
-- appear in the AWS Storage Gateway console. In response, AWS Storage
-- Gateway returns you a snapshot ID. You can use this snapshot ID to check
-- the snapshot progress or later use it when you want to create a volume
-- from a snapshot.
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. For more
-- information, in /Amazon Elastic Compute Cloud API Reference/.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_CreateSnapshotFromVolumeRecoveryPoint.html>
module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
    (
    -- * Request
      CreateSnapshotFromVolumeRecoveryPoint
    -- ** Request constructor
    , createSnapshotFromVolumeRecoveryPoint
    -- ** Request lenses
    , csfvrpVolumeARN
    , csfvrpSnapshotDescription

    -- * Response
    , CreateSnapshotFromVolumeRecoveryPointResponse
    -- ** Response constructor
    , createSnapshotFromVolumeRecoveryPointResponse
    -- ** Response lenses
    , csfvrprsVolumeRecoveryPointTime
    , csfvrprsVolumeARN
    , csfvrprsSnapshotId
    , csfvrprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'createSnapshotFromVolumeRecoveryPoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfvrpVolumeARN'
--
-- * 'csfvrpSnapshotDescription'
data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint'
    { _csfvrpVolumeARN           :: !Text
    , _csfvrpSnapshotDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotFromVolumeRecoveryPoint' smart constructor.
createSnapshotFromVolumeRecoveryPoint :: Text -> Text -> CreateSnapshotFromVolumeRecoveryPoint
createSnapshotFromVolumeRecoveryPoint pVolumeARN_ pSnapshotDescription_ =
    CreateSnapshotFromVolumeRecoveryPoint'
    { _csfvrpVolumeARN = pVolumeARN_
    , _csfvrpSnapshotDescription = pSnapshotDescription_
    }

-- | FIXME: Undocumented member.
csfvrpVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpVolumeARN = lens _csfvrpVolumeARN (\ s a -> s{_csfvrpVolumeARN = a});

-- | FIXME: Undocumented member.
csfvrpSnapshotDescription :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpSnapshotDescription = lens _csfvrpSnapshotDescription (\ s a -> s{_csfvrpSnapshotDescription = a});

instance AWSRequest
         CreateSnapshotFromVolumeRecoveryPoint where
        type Sv CreateSnapshotFromVolumeRecoveryPoint =
             StorageGateway
        type Rs CreateSnapshotFromVolumeRecoveryPoint =
             CreateSnapshotFromVolumeRecoveryPointResponse
        request
          = postJSON "CreateSnapshotFromVolumeRecoveryPoint"
        response
          = receiveJSON
              (\ s h x ->
                 CreateSnapshotFromVolumeRecoveryPointResponse' <$>
                   (x .?> "VolumeRecoveryPointTime") <*>
                     (x .?> "VolumeARN")
                     <*> (x .?> "SnapshotId")
                     <*> (pure (fromEnum s)))

instance ToHeaders
         CreateSnapshotFromVolumeRecoveryPoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.CreateSnapshotFromVolumeRecoveryPoint"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSnapshotFromVolumeRecoveryPoint
         where
        toJSON CreateSnapshotFromVolumeRecoveryPoint'{..}
          = object
              ["VolumeARN" .= _csfvrpVolumeARN,
               "SnapshotDescription" .= _csfvrpSnapshotDescription]

instance ToPath CreateSnapshotFromVolumeRecoveryPoint
         where
        toPath = const "/"

instance ToQuery
         CreateSnapshotFromVolumeRecoveryPoint where
        toQuery = const mempty

-- | /See:/ 'createSnapshotFromVolumeRecoveryPointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfvrprsVolumeRecoveryPointTime'
--
-- * 'csfvrprsVolumeARN'
--
-- * 'csfvrprsSnapshotId'
--
-- * 'csfvrprsStatus'
data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse'
    { _csfvrprsVolumeRecoveryPointTime :: !(Maybe Text)
    , _csfvrprsVolumeARN               :: !(Maybe Text)
    , _csfvrprsSnapshotId              :: !(Maybe Text)
    , _csfvrprsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotFromVolumeRecoveryPointResponse' smart constructor.
createSnapshotFromVolumeRecoveryPointResponse :: Int -> CreateSnapshotFromVolumeRecoveryPointResponse
createSnapshotFromVolumeRecoveryPointResponse pStatus_ =
    CreateSnapshotFromVolumeRecoveryPointResponse'
    { _csfvrprsVolumeRecoveryPointTime = Nothing
    , _csfvrprsVolumeARN = Nothing
    , _csfvrprsSnapshotId = Nothing
    , _csfvrprsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
csfvrprsVolumeRecoveryPointTime :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsVolumeRecoveryPointTime = lens _csfvrprsVolumeRecoveryPointTime (\ s a -> s{_csfvrprsVolumeRecoveryPointTime = a});

-- | FIXME: Undocumented member.
csfvrprsVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsVolumeARN = lens _csfvrprsVolumeARN (\ s a -> s{_csfvrprsVolumeARN = a});

-- | FIXME: Undocumented member.
csfvrprsSnapshotId :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsSnapshotId = lens _csfvrprsSnapshotId (\ s a -> s{_csfvrprsSnapshotId = a});

-- | FIXME: Undocumented member.
csfvrprsStatus :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse Int
csfvrprsStatus = lens _csfvrprsStatus (\ s a -> s{_csfvrprsStatus = a});
