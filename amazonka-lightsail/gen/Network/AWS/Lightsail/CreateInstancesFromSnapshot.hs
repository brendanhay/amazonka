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
-- Module      : Network.AWS.Lightsail.CreateInstancesFromSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses a specific snapshot as a blueprint for creating one or more new instances that are based on that identical configuration.
--
--
module Network.AWS.Lightsail.CreateInstancesFromSnapshot
    (
    -- * Creating a Request
      createInstancesFromSnapshot
    , CreateInstancesFromSnapshot
    -- * Request Lenses
    , cifsUserData
    , cifsKeyPairName
    , cifsAttachedDiskMapping
    , cifsInstanceNames
    , cifsAvailabilityZone
    , cifsInstanceSnapshotName
    , cifsBundleId

    -- * Destructuring the Response
    , createInstancesFromSnapshotResponse
    , CreateInstancesFromSnapshotResponse
    -- * Response Lenses
    , cifsrsOperations
    , cifsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstancesFromSnapshot' smart constructor.
data CreateInstancesFromSnapshot = CreateInstancesFromSnapshot'
  { _cifsUserData             :: !(Maybe Text)
  , _cifsKeyPairName          :: !(Maybe Text)
  , _cifsAttachedDiskMapping  :: !(Maybe (Map Text [DiskMap]))
  , _cifsInstanceNames        :: ![Text]
  , _cifsAvailabilityZone     :: !Text
  , _cifsInstanceSnapshotName :: !Text
  , _cifsBundleId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstancesFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cifsUserData' - You can create a launch script that configures a server with additional user data. For example, @apt-get -y update@ .
--
-- * 'cifsKeyPairName' - The name for your key pair.
--
-- * 'cifsAttachedDiskMapping' - An object containing information about one or more disk mappings.
--
-- * 'cifsInstanceNames' - The names for your new instances.
--
-- * 'cifsAvailabilityZone' - The Availability Zone where you want to create your instances. Use the following formatting: @us-east-2a@ (case sensitive). You can get a list of availability zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include availability zones@ parameter to your request.
--
-- * 'cifsInstanceSnapshotName' - The name of the instance snapshot on which you are basing your new instances. Use the get instance snapshots operation to return information about your existing snapshots.
--
-- * 'cifsBundleId' - The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
createInstancesFromSnapshot
    :: Text -- ^ 'cifsAvailabilityZone'
    -> Text -- ^ 'cifsInstanceSnapshotName'
    -> Text -- ^ 'cifsBundleId'
    -> CreateInstancesFromSnapshot
createInstancesFromSnapshot pAvailabilityZone_ pInstanceSnapshotName_ pBundleId_ =
  CreateInstancesFromSnapshot'
    { _cifsUserData = Nothing
    , _cifsKeyPairName = Nothing
    , _cifsAttachedDiskMapping = Nothing
    , _cifsInstanceNames = mempty
    , _cifsAvailabilityZone = pAvailabilityZone_
    , _cifsInstanceSnapshotName = pInstanceSnapshotName_
    , _cifsBundleId = pBundleId_
    }


-- | You can create a launch script that configures a server with additional user data. For example, @apt-get -y update@ .
cifsUserData :: Lens' CreateInstancesFromSnapshot (Maybe Text)
cifsUserData = lens _cifsUserData (\ s a -> s{_cifsUserData = a})

-- | The name for your key pair.
cifsKeyPairName :: Lens' CreateInstancesFromSnapshot (Maybe Text)
cifsKeyPairName = lens _cifsKeyPairName (\ s a -> s{_cifsKeyPairName = a})

-- | An object containing information about one or more disk mappings.
cifsAttachedDiskMapping :: Lens' CreateInstancesFromSnapshot (HashMap Text [DiskMap])
cifsAttachedDiskMapping = lens _cifsAttachedDiskMapping (\ s a -> s{_cifsAttachedDiskMapping = a}) . _Default . _Map

-- | The names for your new instances.
cifsInstanceNames :: Lens' CreateInstancesFromSnapshot [Text]
cifsInstanceNames = lens _cifsInstanceNames (\ s a -> s{_cifsInstanceNames = a}) . _Coerce

-- | The Availability Zone where you want to create your instances. Use the following formatting: @us-east-2a@ (case sensitive). You can get a list of availability zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include availability zones@ parameter to your request.
cifsAvailabilityZone :: Lens' CreateInstancesFromSnapshot Text
cifsAvailabilityZone = lens _cifsAvailabilityZone (\ s a -> s{_cifsAvailabilityZone = a})

-- | The name of the instance snapshot on which you are basing your new instances. Use the get instance snapshots operation to return information about your existing snapshots.
cifsInstanceSnapshotName :: Lens' CreateInstancesFromSnapshot Text
cifsInstanceSnapshotName = lens _cifsInstanceSnapshotName (\ s a -> s{_cifsInstanceSnapshotName = a})

-- | The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
cifsBundleId :: Lens' CreateInstancesFromSnapshot Text
cifsBundleId = lens _cifsBundleId (\ s a -> s{_cifsBundleId = a})

instance AWSRequest CreateInstancesFromSnapshot where
        type Rs CreateInstancesFromSnapshot =
             CreateInstancesFromSnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateInstancesFromSnapshotResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateInstancesFromSnapshot where

instance NFData CreateInstancesFromSnapshot where

instance ToHeaders CreateInstancesFromSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateInstancesFromSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateInstancesFromSnapshot where
        toJSON CreateInstancesFromSnapshot'{..}
          = object
              (catMaybes
                 [("userData" .=) <$> _cifsUserData,
                  ("keyPairName" .=) <$> _cifsKeyPairName,
                  ("attachedDiskMapping" .=) <$>
                    _cifsAttachedDiskMapping,
                  Just ("instanceNames" .= _cifsInstanceNames),
                  Just ("availabilityZone" .= _cifsAvailabilityZone),
                  Just
                    ("instanceSnapshotName" .=
                       _cifsInstanceSnapshotName),
                  Just ("bundleId" .= _cifsBundleId)])

instance ToPath CreateInstancesFromSnapshot where
        toPath = const "/"

instance ToQuery CreateInstancesFromSnapshot where
        toQuery = const mempty

-- | /See:/ 'createInstancesFromSnapshotResponse' smart constructor.
data CreateInstancesFromSnapshotResponse = CreateInstancesFromSnapshotResponse'
  { _cifsrsOperations     :: !(Maybe [Operation])
  , _cifsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstancesFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cifsrsOperations' - An array of key-value pairs containing information about the results of your create instances from snapshot request.
--
-- * 'cifsrsResponseStatus' - -- | The response status code.
createInstancesFromSnapshotResponse
    :: Int -- ^ 'cifsrsResponseStatus'
    -> CreateInstancesFromSnapshotResponse
createInstancesFromSnapshotResponse pResponseStatus_ =
  CreateInstancesFromSnapshotResponse'
    {_cifsrsOperations = Nothing, _cifsrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your create instances from snapshot request.
cifsrsOperations :: Lens' CreateInstancesFromSnapshotResponse [Operation]
cifsrsOperations = lens _cifsrsOperations (\ s a -> s{_cifsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
cifsrsResponseStatus :: Lens' CreateInstancesFromSnapshotResponse Int
cifsrsResponseStatus = lens _cifsrsResponseStatus (\ s a -> s{_cifsrsResponseStatus = a})

instance NFData CreateInstancesFromSnapshotResponse
         where
