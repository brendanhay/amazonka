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
-- The @create disk@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
--
module Network.AWS.Lightsail.CreateDisk
    (
    -- * Creating a Request
      createDisk
    , CreateDisk
    -- * Request Lenses
    , cTags
    , cDiskName
    , cAvailabilityZone
    , cSizeInGb

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
  { _cTags             :: !(Maybe [Tag])
  , _cDiskName         :: !Text
  , _cAvailabilityZone :: !Text
  , _cSizeInGb         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cTags' - The tag keys and optional values to add to the resource during create. To tag a resource after it has been created, see the @tag resource@ operation.
--
-- * 'cDiskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- * 'cAvailabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- * 'cSizeInGb' - The size of the disk in GB (e.g., @32@ ).
createDisk
    :: Text -- ^ 'cDiskName'
    -> Text -- ^ 'cAvailabilityZone'
    -> Int -- ^ 'cSizeInGb'
    -> CreateDisk
createDisk pDiskName_ pAvailabilityZone_ pSizeInGb_ =
  CreateDisk'
    { _cTags = Nothing
    , _cDiskName = pDiskName_
    , _cAvailabilityZone = pAvailabilityZone_
    , _cSizeInGb = pSizeInGb_
    }


-- | The tag keys and optional values to add to the resource during create. To tag a resource after it has been created, see the @tag resource@ operation.
cTags :: Lens' CreateDisk [Tag]
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Coerce

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
cDiskName :: Lens' CreateDisk Text
cDiskName = lens _cDiskName (\ s a -> s{_cDiskName = a})

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
cAvailabilityZone :: Lens' CreateDisk Text
cAvailabilityZone = lens _cAvailabilityZone (\ s a -> s{_cAvailabilityZone = a})

-- | The size of the disk in GB (e.g., @32@ ).
cSizeInGb :: Lens' CreateDisk Int
cSizeInGb = lens _cSizeInGb (\ s a -> s{_cSizeInGb = a})

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
                 [("tags" .=) <$> _cTags,
                  Just ("diskName" .= _cDiskName),
                  Just ("availabilityZone" .= _cAvailabilityZone),
                  Just ("sizeInGb" .= _cSizeInGb)])

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
