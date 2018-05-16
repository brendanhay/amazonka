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
-- Module      : Network.AWS.Lightsail.CreateDiskFromSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk from a disk snapshot that can be attached to a Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail Regions and Availability Zones in Lightsail> .
--
--
module Network.AWS.Lightsail.CreateDiskFromSnapshot
    (
    -- * Creating a Request
      createDiskFromSnapshot
    , CreateDiskFromSnapshot
    -- * Request Lenses
    , cdfsDiskName
    , cdfsDiskSnapshotName
    , cdfsAvailabilityZone
    , cdfsSizeInGb

    -- * Destructuring the Response
    , createDiskFromSnapshotResponse
    , CreateDiskFromSnapshotResponse
    -- * Response Lenses
    , cdfsrsOperations
    , cdfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { _cdfsDiskName         :: !Text
  , _cdfsDiskSnapshotName :: !Text
  , _cdfsAvailabilityZone :: !Text
  , _cdfsSizeInGb         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDiskFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdfsDiskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- * 'cdfsDiskSnapshotName' - The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
--
-- * 'cdfsAvailabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- * 'cdfsSizeInGb' - The size of the disk in GB (e.g., @32@ ).
createDiskFromSnapshot
    :: Text -- ^ 'cdfsDiskName'
    -> Text -- ^ 'cdfsDiskSnapshotName'
    -> Text -- ^ 'cdfsAvailabilityZone'
    -> Int -- ^ 'cdfsSizeInGb'
    -> CreateDiskFromSnapshot
createDiskFromSnapshot pDiskName_ pDiskSnapshotName_ pAvailabilityZone_ pSizeInGb_ =
  CreateDiskFromSnapshot'
    { _cdfsDiskName = pDiskName_
    , _cdfsDiskSnapshotName = pDiskSnapshotName_
    , _cdfsAvailabilityZone = pAvailabilityZone_
    , _cdfsSizeInGb = pSizeInGb_
    }


-- | The unique Lightsail disk name (e.g., @my-disk@ ).
cdfsDiskName :: Lens' CreateDiskFromSnapshot Text
cdfsDiskName = lens _cdfsDiskName (\ s a -> s{_cdfsDiskName = a})

-- | The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
cdfsDiskSnapshotName :: Lens' CreateDiskFromSnapshot Text
cdfsDiskSnapshotName = lens _cdfsDiskSnapshotName (\ s a -> s{_cdfsDiskSnapshotName = a})

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
cdfsAvailabilityZone :: Lens' CreateDiskFromSnapshot Text
cdfsAvailabilityZone = lens _cdfsAvailabilityZone (\ s a -> s{_cdfsAvailabilityZone = a})

-- | The size of the disk in GB (e.g., @32@ ).
cdfsSizeInGb :: Lens' CreateDiskFromSnapshot Int
cdfsSizeInGb = lens _cdfsSizeInGb (\ s a -> s{_cdfsSizeInGb = a})

instance AWSRequest CreateDiskFromSnapshot where
        type Rs CreateDiskFromSnapshot =
             CreateDiskFromSnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateDiskFromSnapshotResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateDiskFromSnapshot where

instance NFData CreateDiskFromSnapshot where

instance ToHeaders CreateDiskFromSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateDiskFromSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDiskFromSnapshot where
        toJSON CreateDiskFromSnapshot'{..}
          = object
              (catMaybes
                 [Just ("diskName" .= _cdfsDiskName),
                  Just ("diskSnapshotName" .= _cdfsDiskSnapshotName),
                  Just ("availabilityZone" .= _cdfsAvailabilityZone),
                  Just ("sizeInGb" .= _cdfsSizeInGb)])

instance ToPath CreateDiskFromSnapshot where
        toPath = const "/"

instance ToQuery CreateDiskFromSnapshot where
        toQuery = const mempty

-- | /See:/ 'createDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
  { _cdfsrsOperations     :: !(Maybe [Operation])
  , _cdfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDiskFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdfsrsOperations' - An object describing the API operations.
--
-- * 'cdfsrsResponseStatus' - -- | The response status code.
createDiskFromSnapshotResponse
    :: Int -- ^ 'cdfsrsResponseStatus'
    -> CreateDiskFromSnapshotResponse
createDiskFromSnapshotResponse pResponseStatus_ =
  CreateDiskFromSnapshotResponse'
    {_cdfsrsOperations = Nothing, _cdfsrsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
cdfsrsOperations :: Lens' CreateDiskFromSnapshotResponse [Operation]
cdfsrsOperations = lens _cdfsrsOperations (\ s a -> s{_cdfsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
cdfsrsResponseStatus :: Lens' CreateDiskFromSnapshotResponse Int
cdfsrsResponseStatus = lens _cdfsrsResponseStatus (\ s a -> s{_cdfsrsResponseStatus = a})

instance NFData CreateDiskFromSnapshotResponse where
