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
-- Module      : Network.AWS.Lightsail.CreateDiskFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk from a manual or automatic snapshot of a disk. The resulting disk can be attached to an Amazon Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ).
--
--
-- The @create disk from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @disk snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDiskFromSnapshot
  ( -- * Creating a Request
    createDiskFromSnapshot,
    CreateDiskFromSnapshot,

    -- * Request Lenses
    cdfsUseLatestRestorableAutoSnapshot,
    cdfsSourceDiskName,
    cdfsAddOns,
    cdfsDiskSnapshotName,
    cdfsRestoreDate,
    cdfsTags,
    cdfsDiskName,
    cdfsAvailabilityZone,
    cdfsSizeInGb,

    -- * Destructuring the Response
    createDiskFromSnapshotResponse,
    CreateDiskFromSnapshotResponse,

    -- * Response Lenses
    cdfsrsOperations,
    cdfsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { _cdfsUseLatestRestorableAutoSnapshot ::
      !(Maybe Bool),
    _cdfsSourceDiskName :: !(Maybe Text),
    _cdfsAddOns :: !(Maybe [AddOnRequest]),
    _cdfsDiskSnapshotName :: !(Maybe Text),
    _cdfsRestoreDate :: !(Maybe Text),
    _cdfsTags :: !(Maybe [Tag]),
    _cdfsDiskName :: !Text,
    _cdfsAvailabilityZone :: !Text,
    _cdfsSizeInGb :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDiskFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdfsUseLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available automatic snapshot. Constraints:     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
-- * 'cdfsSourceDiskName' - The name of the source disk from which the source automatic snapshot was created. Constraints:     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
-- * 'cdfsAddOns' - An array of objects that represent the add-ons to enable for the new disk.
--
-- * 'cdfsDiskSnapshotName' - The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk. Constraint:     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
--
-- * 'cdfsRestoreDate' - The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots. Constraints:     * Must be specified in @YYYY-MM-DD@ format.     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
-- * 'cdfsTags' - The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
--
-- * 'cdfsDiskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- * 'cdfsAvailabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- * 'cdfsSizeInGb' - The size of the disk in GB (e.g., @32@ ).
createDiskFromSnapshot ::
  -- | 'cdfsDiskName'
  Text ->
  -- | 'cdfsAvailabilityZone'
  Text ->
  -- | 'cdfsSizeInGb'
  Int ->
  CreateDiskFromSnapshot
createDiskFromSnapshot pDiskName_ pAvailabilityZone_ pSizeInGb_ =
  CreateDiskFromSnapshot'
    { _cdfsUseLatestRestorableAutoSnapshot =
        Nothing,
      _cdfsSourceDiskName = Nothing,
      _cdfsAddOns = Nothing,
      _cdfsDiskSnapshotName = Nothing,
      _cdfsRestoreDate = Nothing,
      _cdfsTags = Nothing,
      _cdfsDiskName = pDiskName_,
      _cdfsAvailabilityZone = pAvailabilityZone_,
      _cdfsSizeInGb = pSizeInGb_
    }

-- | A Boolean value to indicate whether to use the latest available automatic snapshot. Constraints:     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
cdfsUseLatestRestorableAutoSnapshot :: Lens' CreateDiskFromSnapshot (Maybe Bool)
cdfsUseLatestRestorableAutoSnapshot = lens _cdfsUseLatestRestorableAutoSnapshot (\s a -> s {_cdfsUseLatestRestorableAutoSnapshot = a})

-- | The name of the source disk from which the source automatic snapshot was created. Constraints:     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
cdfsSourceDiskName :: Lens' CreateDiskFromSnapshot (Maybe Text)
cdfsSourceDiskName = lens _cdfsSourceDiskName (\s a -> s {_cdfsSourceDiskName = a})

-- | An array of objects that represent the add-ons to enable for the new disk.
cdfsAddOns :: Lens' CreateDiskFromSnapshot [AddOnRequest]
cdfsAddOns = lens _cdfsAddOns (\s a -> s {_cdfsAddOns = a}) . _Default . _Coerce

-- | The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk. Constraint:     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
cdfsDiskSnapshotName :: Lens' CreateDiskFromSnapshot (Maybe Text)
cdfsDiskSnapshotName = lens _cdfsDiskSnapshotName (\s a -> s {_cdfsDiskSnapshotName = a})

-- | The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots. Constraints:     * Must be specified in @YYYY-MM-DD@ format.     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
cdfsRestoreDate :: Lens' CreateDiskFromSnapshot (Maybe Text)
cdfsRestoreDate = lens _cdfsRestoreDate (\s a -> s {_cdfsRestoreDate = a})

-- | The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
cdfsTags :: Lens' CreateDiskFromSnapshot [Tag]
cdfsTags = lens _cdfsTags (\s a -> s {_cdfsTags = a}) . _Default . _Coerce

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
cdfsDiskName :: Lens' CreateDiskFromSnapshot Text
cdfsDiskName = lens _cdfsDiskName (\s a -> s {_cdfsDiskName = a})

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk. Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
cdfsAvailabilityZone :: Lens' CreateDiskFromSnapshot Text
cdfsAvailabilityZone = lens _cdfsAvailabilityZone (\s a -> s {_cdfsAvailabilityZone = a})

-- | The size of the disk in GB (e.g., @32@ ).
cdfsSizeInGb :: Lens' CreateDiskFromSnapshot Int
cdfsSizeInGb = lens _cdfsSizeInGb (\s a -> s {_cdfsSizeInGb = a})

instance AWSRequest CreateDiskFromSnapshot where
  type Rs CreateDiskFromSnapshot = CreateDiskFromSnapshotResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateDiskFromSnapshotResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateDiskFromSnapshot

instance NFData CreateDiskFromSnapshot

instance ToHeaders CreateDiskFromSnapshot where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateDiskFromSnapshot" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDiskFromSnapshot where
  toJSON CreateDiskFromSnapshot' {..} =
    object
      ( catMaybes
          [ ("useLatestRestorableAutoSnapshot" .=)
              <$> _cdfsUseLatestRestorableAutoSnapshot,
            ("sourceDiskName" .=) <$> _cdfsSourceDiskName,
            ("addOns" .=) <$> _cdfsAddOns,
            ("diskSnapshotName" .=) <$> _cdfsDiskSnapshotName,
            ("restoreDate" .=) <$> _cdfsRestoreDate,
            ("tags" .=) <$> _cdfsTags,
            Just ("diskName" .= _cdfsDiskName),
            Just ("availabilityZone" .= _cdfsAvailabilityZone),
            Just ("sizeInGb" .= _cdfsSizeInGb)
          ]
      )

instance ToPath CreateDiskFromSnapshot where
  toPath = const "/"

instance ToQuery CreateDiskFromSnapshot where
  toQuery = const mempty

-- | /See:/ 'createDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
  { _cdfsrsOperations ::
      !(Maybe [Operation]),
    _cdfsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDiskFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdfsrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'cdfsrsResponseStatus' - -- | The response status code.
createDiskFromSnapshotResponse ::
  -- | 'cdfsrsResponseStatus'
  Int ->
  CreateDiskFromSnapshotResponse
createDiskFromSnapshotResponse pResponseStatus_ =
  CreateDiskFromSnapshotResponse'
    { _cdfsrsOperations = Nothing,
      _cdfsrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
cdfsrsOperations :: Lens' CreateDiskFromSnapshotResponse [Operation]
cdfsrsOperations = lens _cdfsrsOperations (\s a -> s {_cdfsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
cdfsrsResponseStatus :: Lens' CreateDiskFromSnapshotResponse Int
cdfsrsResponseStatus = lens _cdfsrsResponseStatus (\s a -> s {_cdfsrsResponseStatus = a})

instance NFData CreateDiskFromSnapshotResponse
