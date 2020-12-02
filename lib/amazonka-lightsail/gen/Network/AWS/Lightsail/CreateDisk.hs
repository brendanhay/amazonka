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
-- Module      : Network.AWS.Lightsail.CreateDisk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk that can be attached to a Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ). The disk is created in the regional endpoint that you send the HTTP request to. For more information, see <https://lightsail.aws.amazon.com/ls/docs/overview/article/understanding-regions-and-availability-zones-in-amazon-lightsail Regions and Availability Zones in Lightsail> .
--
--
module Network.AWS.Lightsail.CreateDisk
    (
    -- * Creating a Request
      createDisk
    , CreateDisk
    -- * Request Lenses
    , cdDiskName
    , cdAvailabilityZone
    , cdSizeInGb

    -- * Destructuring the Response
    , createDiskResponse
    , CreateDiskResponse
    -- * Response Lenses
    , crsOperations
    , crsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDisk' smart constructor.
data CreateDisk = CreateDisk'
  { _cdDiskName         :: !Text
  , _cdAvailabilityZone :: !Text
  , _cdSizeInGb         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDiskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- * 'cdAvailabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- * 'cdSizeInGb' - The size of the disk in GB (e.g., @32@ ).
createDisk
    :: Text -- ^ 'cdDiskName'
    -> Text -- ^ 'cdAvailabilityZone'
    -> Int -- ^ 'cdSizeInGb'
    -> CreateDisk
createDisk pDiskName_ pAvailabilityZone_ pSizeInGb_ =
  CreateDisk'
    { _cdDiskName = pDiskName_
    , _cdAvailabilityZone = pAvailabilityZone_
    , _cdSizeInGb = pSizeInGb_
    }


-- | The unique Lightsail disk name (e.g., @my-disk@ ).
cdDiskName :: Lens' CreateDisk Text
cdDiskName = lens _cdDiskName (\ s a -> s{_cdDiskName = a})

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
cdAvailabilityZone :: Lens' CreateDisk Text
cdAvailabilityZone = lens _cdAvailabilityZone (\ s a -> s{_cdAvailabilityZone = a})

-- | The size of the disk in GB (e.g., @32@ ).
cdSizeInGb :: Lens' CreateDisk Int
cdSizeInGb = lens _cdSizeInGb (\ s a -> s{_cdSizeInGb = a})

instance AWSRequest CreateDisk where
        type Rs CreateDisk = CreateDiskResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateDiskResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateDisk where

instance NFData CreateDisk where

instance ToHeaders CreateDisk where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateDisk" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDisk where
        toJSON CreateDisk'{..}
          = object
              (catMaybes
                 [Just ("diskName" .= _cdDiskName),
                  Just ("availabilityZone" .= _cdAvailabilityZone),
                  Just ("sizeInGb" .= _cdSizeInGb)])

instance ToPath CreateDisk where
        toPath = const "/"

instance ToQuery CreateDisk where
        toQuery = const mempty

-- | /See:/ 'createDiskResponse' smart constructor.
data CreateDiskResponse = CreateDiskResponse'
  { _crsOperations     :: !(Maybe [Operation])
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsOperations' - An object describing the API operations.
--
-- * 'crsResponseStatus' - -- | The response status code.
createDiskResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateDiskResponse
createDiskResponse pResponseStatus_ =
  CreateDiskResponse'
    {_crsOperations = Nothing, _crsResponseStatus = pResponseStatus_}


-- | An object describing the API operations.
crsOperations :: Lens' CreateDiskResponse [Operation]
crsOperations = lens _crsOperations (\ s a -> s{_crsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateDiskResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateDiskResponse where
