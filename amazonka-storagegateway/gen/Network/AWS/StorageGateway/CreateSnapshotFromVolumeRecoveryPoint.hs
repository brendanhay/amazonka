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
-- Module      : Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a snapshot of a gateway from a volume recovery point. This operation is only supported in the cached volume gateway type.
--
--
-- A volume recovery point is a point in time at which all data of the volume is consistent and from which you can create a snapshot. To get a list of volume recovery point for cached volume gateway, use 'ListVolumeRecoveryPoints' .
--
-- In the @CreateSnapshotFromVolumeRecoveryPoint@ request, you identify the volume by providing its Amazon Resource Name (ARN). You must also provide a description for the snapshot. When the gateway takes a snapshot of the specified volume, the snapshot and its description appear in the AWS Storage Gateway console. In response, the gateway returns you a snapshot ID. You can use this snapshot ID to check the snapshot progress or later use it when you want to create a volume from a snapshot.
--
module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
    (
    -- * Creating a Request
      createSnapshotFromVolumeRecoveryPoint
    , CreateSnapshotFromVolumeRecoveryPoint
    -- * Request Lenses
    , csfvrpVolumeARN
    , csfvrpSnapshotDescription

    -- * Destructuring the Response
    , createSnapshotFromVolumeRecoveryPointResponse
    , CreateSnapshotFromVolumeRecoveryPointResponse
    -- * Response Lenses
    , csfvrprsVolumeRecoveryPointTime
    , csfvrprsVolumeARN
    , csfvrprsSnapshotId
    , csfvrprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'createSnapshotFromVolumeRecoveryPoint' smart constructor.
data CreateSnapshotFromVolumeRecoveryPoint = CreateSnapshotFromVolumeRecoveryPoint'
  { _csfvrpVolumeARN           :: !Text
  , _csfvrpSnapshotDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshotFromVolumeRecoveryPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfvrpVolumeARN' - Undocumented member.
--
-- * 'csfvrpSnapshotDescription' - Undocumented member.
createSnapshotFromVolumeRecoveryPoint
    :: Text -- ^ 'csfvrpVolumeARN'
    -> Text -- ^ 'csfvrpSnapshotDescription'
    -> CreateSnapshotFromVolumeRecoveryPoint
createSnapshotFromVolumeRecoveryPoint pVolumeARN_ pSnapshotDescription_ =
  CreateSnapshotFromVolumeRecoveryPoint'
    { _csfvrpVolumeARN = pVolumeARN_
    , _csfvrpSnapshotDescription = pSnapshotDescription_
    }


-- | Undocumented member.
csfvrpVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpVolumeARN = lens _csfvrpVolumeARN (\ s a -> s{_csfvrpVolumeARN = a})

-- | Undocumented member.
csfvrpSnapshotDescription :: Lens' CreateSnapshotFromVolumeRecoveryPoint Text
csfvrpSnapshotDescription = lens _csfvrpSnapshotDescription (\ s a -> s{_csfvrpSnapshotDescription = a})

instance AWSRequest
           CreateSnapshotFromVolumeRecoveryPoint
         where
        type Rs CreateSnapshotFromVolumeRecoveryPoint =
             CreateSnapshotFromVolumeRecoveryPointResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 CreateSnapshotFromVolumeRecoveryPointResponse' <$>
                   (x .?> "VolumeRecoveryPointTime") <*>
                     (x .?> "VolumeARN")
                     <*> (x .?> "SnapshotId")
                     <*> (pure (fromEnum s)))

instance Hashable
           CreateSnapshotFromVolumeRecoveryPoint
         where

instance NFData CreateSnapshotFromVolumeRecoveryPoint
         where

instance ToHeaders
           CreateSnapshotFromVolumeRecoveryPoint
         where
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
              (catMaybes
                 [Just ("VolumeARN" .= _csfvrpVolumeARN),
                  Just
                    ("SnapshotDescription" .=
                       _csfvrpSnapshotDescription)])

instance ToPath CreateSnapshotFromVolumeRecoveryPoint
         where
        toPath = const "/"

instance ToQuery
           CreateSnapshotFromVolumeRecoveryPoint
         where
        toQuery = const mempty

-- | /See:/ 'createSnapshotFromVolumeRecoveryPointResponse' smart constructor.
data CreateSnapshotFromVolumeRecoveryPointResponse = CreateSnapshotFromVolumeRecoveryPointResponse'
  { _csfvrprsVolumeRecoveryPointTime :: !(Maybe Text)
  , _csfvrprsVolumeARN               :: !(Maybe Text)
  , _csfvrprsSnapshotId              :: !(Maybe Text)
  , _csfvrprsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshotFromVolumeRecoveryPointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csfvrprsVolumeRecoveryPointTime' - Undocumented member.
--
-- * 'csfvrprsVolumeARN' - Undocumented member.
--
-- * 'csfvrprsSnapshotId' - Undocumented member.
--
-- * 'csfvrprsResponseStatus' - -- | The response status code.
createSnapshotFromVolumeRecoveryPointResponse
    :: Int -- ^ 'csfvrprsResponseStatus'
    -> CreateSnapshotFromVolumeRecoveryPointResponse
createSnapshotFromVolumeRecoveryPointResponse pResponseStatus_ =
  CreateSnapshotFromVolumeRecoveryPointResponse'
    { _csfvrprsVolumeRecoveryPointTime = Nothing
    , _csfvrprsVolumeARN = Nothing
    , _csfvrprsSnapshotId = Nothing
    , _csfvrprsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
csfvrprsVolumeRecoveryPointTime :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsVolumeRecoveryPointTime = lens _csfvrprsVolumeRecoveryPointTime (\ s a -> s{_csfvrprsVolumeRecoveryPointTime = a})

-- | Undocumented member.
csfvrprsVolumeARN :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsVolumeARN = lens _csfvrprsVolumeARN (\ s a -> s{_csfvrprsVolumeARN = a})

-- | Undocumented member.
csfvrprsSnapshotId :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse (Maybe Text)
csfvrprsSnapshotId = lens _csfvrprsSnapshotId (\ s a -> s{_csfvrprsSnapshotId = a})

-- | -- | The response status code.
csfvrprsResponseStatus :: Lens' CreateSnapshotFromVolumeRecoveryPointResponse Int
csfvrprsResponseStatus = lens _csfvrprsResponseStatus (\ s a -> s{_csfvrprsResponseStatus = a})

instance NFData
           CreateSnapshotFromVolumeRecoveryPointResponse
         where
