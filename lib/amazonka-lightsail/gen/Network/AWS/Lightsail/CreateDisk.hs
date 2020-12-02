{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk that can be attached to an Amazon Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ).
--
--
-- The @create disk@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDisk
  ( -- * Creating a Request
    createDisk,
    CreateDisk,

    -- * Request Lenses
    cddAddOns,
    cddTags,
    cddDiskName,
    cddAvailabilityZone,
    cddSizeInGb,

    -- * Destructuring the Response
    createDiskResponse,
    CreateDiskResponse,

    -- * Response Lenses
    crsOperations,
    crsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDisk' smart constructor.
data CreateDisk = CreateDisk'
  { _cddAddOns ::
      !(Maybe [AddOnRequest]),
    _cddTags :: !(Maybe [Tag]),
    _cddDiskName :: !Text,
    _cddAvailabilityZone :: !Text,
    _cddSizeInGb :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDisk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cddAddOns' - An array of objects that represent the add-ons to enable for the new disk.
--
-- * 'cddTags' - The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
--
-- * 'cddDiskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- * 'cddAvailabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk. Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
--
-- * 'cddSizeInGb' - The size of the disk in GB (e.g., @32@ ).
createDisk ::
  -- | 'cddDiskName'
  Text ->
  -- | 'cddAvailabilityZone'
  Text ->
  -- | 'cddSizeInGb'
  Int ->
  CreateDisk
createDisk pDiskName_ pAvailabilityZone_ pSizeInGb_ =
  CreateDisk'
    { _cddAddOns = Nothing,
      _cddTags = Nothing,
      _cddDiskName = pDiskName_,
      _cddAvailabilityZone = pAvailabilityZone_,
      _cddSizeInGb = pSizeInGb_
    }

-- | An array of objects that represent the add-ons to enable for the new disk.
cddAddOns :: Lens' CreateDisk [AddOnRequest]
cddAddOns = lens _cddAddOns (\s a -> s {_cddAddOns = a}) . _Default . _Coerce

-- | The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
cddTags :: Lens' CreateDisk [Tag]
cddTags = lens _cddTags (\s a -> s {_cddTags = a}) . _Default . _Coerce

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
cddDiskName :: Lens' CreateDisk Text
cddDiskName = lens _cddDiskName (\s a -> s {_cddDiskName = a})

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk. Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
cddAvailabilityZone :: Lens' CreateDisk Text
cddAvailabilityZone = lens _cddAvailabilityZone (\s a -> s {_cddAvailabilityZone = a})

-- | The size of the disk in GB (e.g., @32@ ).
cddSizeInGb :: Lens' CreateDisk Int
cddSizeInGb = lens _cddSizeInGb (\s a -> s {_cddSizeInGb = a})

instance AWSRequest CreateDisk where
  type Rs CreateDisk = CreateDiskResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateDiskResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateDisk

instance NFData CreateDisk

instance ToHeaders CreateDisk where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.CreateDisk" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDisk where
  toJSON CreateDisk' {..} =
    object
      ( catMaybes
          [ ("addOns" .=) <$> _cddAddOns,
            ("tags" .=) <$> _cddTags,
            Just ("diskName" .= _cddDiskName),
            Just ("availabilityZone" .= _cddAvailabilityZone),
            Just ("sizeInGb" .= _cddSizeInGb)
          ]
      )

instance ToPath CreateDisk where
  toPath = const "/"

instance ToQuery CreateDisk where
  toQuery = const mempty

-- | /See:/ 'createDiskResponse' smart constructor.
data CreateDiskResponse = CreateDiskResponse'
  { _crsOperations ::
      !(Maybe [Operation]),
    _crsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDiskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'crsResponseStatus' - -- | The response status code.
createDiskResponse ::
  -- | 'crsResponseStatus'
  Int ->
  CreateDiskResponse
createDiskResponse pResponseStatus_ =
  CreateDiskResponse'
    { _crsOperations = Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
crsOperations :: Lens' CreateDiskResponse [Operation]
crsOperations = lens _crsOperations (\s a -> s {_crsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateDiskResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData CreateDiskResponse
