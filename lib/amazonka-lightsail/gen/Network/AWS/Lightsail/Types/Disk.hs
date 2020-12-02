{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Disk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Disk where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.DiskState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes a system disk or a block storage disk.
--
--
--
-- /See:/ 'disk' smart constructor.
data Disk = Disk'
  { _dState :: !(Maybe DiskState),
    _dResourceType :: !(Maybe ResourceType),
    _dArn :: !(Maybe Text),
    _dPath :: !(Maybe Text),
    _dCreatedAt :: !(Maybe POSIX),
    _dLocation :: !(Maybe ResourceLocation),
    _dIops :: !(Maybe Int),
    _dIsAttached :: !(Maybe Bool),
    _dAddOns :: !(Maybe [AddOn]),
    _dAttachmentState :: !(Maybe Text),
    _dName :: !(Maybe Text),
    _dSizeInGb :: !(Maybe Int),
    _dSupportCode :: !(Maybe Text),
    _dIsSystemDisk :: !(Maybe Bool),
    _dAttachedTo :: !(Maybe Text),
    _dGbInUse :: !(Maybe Int),
    _dTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dState' - Describes the status of the disk.
--
-- * 'dResourceType' - The Lightsail resource type (e.g., @Disk@ ).
--
-- * 'dArn' - The Amazon Resource Name (ARN) of the disk.
--
-- * 'dPath' - The disk path.
--
-- * 'dCreatedAt' - The date when the disk was created.
--
-- * 'dLocation' - The AWS Region and Availability Zone where the disk is located.
--
-- * 'dIops' - The input/output operations per second (IOPS) of the disk.
--
-- * 'dIsAttached' - A Boolean value indicating whether the disk is attached.
--
-- * 'dAddOns' - An array of objects representing the add-ons enabled on the disk.
--
-- * 'dAttachmentState' - (Deprecated) The attachment state of the disk.
--
-- * 'dName' - The unique name of the disk.
--
-- * 'dSizeInGb' - The size of the disk in GB.
--
-- * 'dSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'dIsSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- * 'dAttachedTo' - The resources to which the disk is attached.
--
-- * 'dGbInUse' - (Deprecated) The number of GB in use by the disk.
--
-- * 'dTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
disk ::
  Disk
disk =
  Disk'
    { _dState = Nothing,
      _dResourceType = Nothing,
      _dArn = Nothing,
      _dPath = Nothing,
      _dCreatedAt = Nothing,
      _dLocation = Nothing,
      _dIops = Nothing,
      _dIsAttached = Nothing,
      _dAddOns = Nothing,
      _dAttachmentState = Nothing,
      _dName = Nothing,
      _dSizeInGb = Nothing,
      _dSupportCode = Nothing,
      _dIsSystemDisk = Nothing,
      _dAttachedTo = Nothing,
      _dGbInUse = Nothing,
      _dTags = Nothing
    }

-- | Describes the status of the disk.
dState :: Lens' Disk (Maybe DiskState)
dState = lens _dState (\s a -> s {_dState = a})

-- | The Lightsail resource type (e.g., @Disk@ ).
dResourceType :: Lens' Disk (Maybe ResourceType)
dResourceType = lens _dResourceType (\s a -> s {_dResourceType = a})

-- | The Amazon Resource Name (ARN) of the disk.
dArn :: Lens' Disk (Maybe Text)
dArn = lens _dArn (\s a -> s {_dArn = a})

-- | The disk path.
dPath :: Lens' Disk (Maybe Text)
dPath = lens _dPath (\s a -> s {_dPath = a})

-- | The date when the disk was created.
dCreatedAt :: Lens' Disk (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\s a -> s {_dCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where the disk is located.
dLocation :: Lens' Disk (Maybe ResourceLocation)
dLocation = lens _dLocation (\s a -> s {_dLocation = a})

-- | The input/output operations per second (IOPS) of the disk.
dIops :: Lens' Disk (Maybe Int)
dIops = lens _dIops (\s a -> s {_dIops = a})

-- | A Boolean value indicating whether the disk is attached.
dIsAttached :: Lens' Disk (Maybe Bool)
dIsAttached = lens _dIsAttached (\s a -> s {_dIsAttached = a})

-- | An array of objects representing the add-ons enabled on the disk.
dAddOns :: Lens' Disk [AddOn]
dAddOns = lens _dAddOns (\s a -> s {_dAddOns = a}) . _Default . _Coerce

-- | (Deprecated) The attachment state of the disk.
dAttachmentState :: Lens' Disk (Maybe Text)
dAttachmentState = lens _dAttachmentState (\s a -> s {_dAttachmentState = a})

-- | The unique name of the disk.
dName :: Lens' Disk (Maybe Text)
dName = lens _dName (\s a -> s {_dName = a})

-- | The size of the disk in GB.
dSizeInGb :: Lens' Disk (Maybe Int)
dSizeInGb = lens _dSizeInGb (\s a -> s {_dSizeInGb = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
dSupportCode :: Lens' Disk (Maybe Text)
dSupportCode = lens _dSupportCode (\s a -> s {_dSupportCode = a})

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
dIsSystemDisk :: Lens' Disk (Maybe Bool)
dIsSystemDisk = lens _dIsSystemDisk (\s a -> s {_dIsSystemDisk = a})

-- | The resources to which the disk is attached.
dAttachedTo :: Lens' Disk (Maybe Text)
dAttachedTo = lens _dAttachedTo (\s a -> s {_dAttachedTo = a})

-- | (Deprecated) The number of GB in use by the disk.
dGbInUse :: Lens' Disk (Maybe Int)
dGbInUse = lens _dGbInUse (\s a -> s {_dGbInUse = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
dTags :: Lens' Disk [Tag]
dTags = lens _dTags (\s a -> s {_dTags = a}) . _Default . _Coerce

instance FromJSON Disk where
  parseJSON =
    withObject
      "Disk"
      ( \x ->
          Disk'
            <$> (x .:? "state")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "path")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "iops")
            <*> (x .:? "isAttached")
            <*> (x .:? "addOns" .!= mempty)
            <*> (x .:? "attachmentState")
            <*> (x .:? "name")
            <*> (x .:? "sizeInGb")
            <*> (x .:? "supportCode")
            <*> (x .:? "isSystemDisk")
            <*> (x .:? "attachedTo")
            <*> (x .:? "gbInUse")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable Disk

instance NFData Disk
