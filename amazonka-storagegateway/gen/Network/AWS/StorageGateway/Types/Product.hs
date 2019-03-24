{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.Sum

-- | Describes an iSCSI cached volume.
--
--
--
-- /See:/ 'cachediSCSIVolume' smart constructor.
data CachediSCSIVolume = CachediSCSIVolume'
  { _cscsivVolumeiSCSIAttributes  :: !(Maybe VolumeiSCSIAttributes)
  , _cscsivVolumeStatus           :: !(Maybe Text)
  , _cscsivSourceSnapshotId       :: !(Maybe Text)
  , _cscsivKMSKey                 :: !(Maybe Text)
  , _cscsivVolumeAttachmentStatus :: !(Maybe Text)
  , _cscsivVolumeARN              :: !(Maybe Text)
  , _cscsivVolumeProgress         :: !(Maybe Double)
  , _cscsivVolumeSizeInBytes      :: !(Maybe Integer)
  , _cscsivVolumeUsedInBytes      :: !(Maybe Integer)
  , _cscsivCreatedDate            :: !(Maybe POSIX)
  , _cscsivVolumeId               :: !(Maybe Text)
  , _cscsivVolumeType             :: !(Maybe Text)
  , _cscsivTargetName             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CachediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsivVolumeiSCSIAttributes' - An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- * 'cscsivVolumeStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
--
-- * 'cscsivSourceSnapshotId' - If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
--
-- * 'cscsivKMSKey' - Undocumented member.
--
-- * 'cscsivVolumeAttachmentStatus' - A value that indicates whether a storage volume is attached to or detached from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving Your Volumes to a Different Gateway> .
--
-- * 'cscsivVolumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- * 'cscsivVolumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
--
-- * 'cscsivVolumeSizeInBytes' - The size, in bytes, of the volume capacity.
--
-- * 'cscsivVolumeUsedInBytes' - The size of the data stored on the volume in bytes.
--
-- * 'cscsivCreatedDate' - The date the volume was created. Volumes created prior to March 28, 2017 don
