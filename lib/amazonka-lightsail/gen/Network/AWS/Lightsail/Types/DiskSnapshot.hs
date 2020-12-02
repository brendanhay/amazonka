{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshot where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes a block storage disk snapshot.
--
--
--
-- /See:/ 'diskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { _dsFromDiskName :: !(Maybe Text),
    _dsIsFromAutoSnapshot :: !(Maybe Bool),
    _dsState :: !(Maybe DiskSnapshotState),
    _dsResourceType :: !(Maybe ResourceType),
    _dsArn :: !(Maybe Text),
    _dsCreatedAt :: !(Maybe POSIX),
    _dsLocation :: !(Maybe ResourceLocation),
    _dsProgress :: !(Maybe Text),
    _dsName :: !(Maybe Text),
    _dsSizeInGb :: !(Maybe Int),
    _dsSupportCode :: !(Maybe Text),
    _dsFromInstanceARN :: !(Maybe Text),
    _dsFromInstanceName :: !(Maybe Text),
    _dsFromDiskARN :: !(Maybe Text),
    _dsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiskSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsFromDiskName' - The unique name of the source disk from which the disk snapshot was created.
--
-- * 'dsIsFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an automatic snapshot.
--
-- * 'dsState' - The status of the disk snapshot operation.
--
-- * 'dsResourceType' - The Lightsail resource type (e.g., @DiskSnapshot@ ).
--
-- * 'dsArn' - The Amazon Resource Name (ARN) of the disk snapshot.
--
-- * 'dsCreatedAt' - The date when the disk snapshot was created.
--
-- * 'dsLocation' - The AWS Region and Availability Zone where the disk snapshot was created.
--
-- * 'dsProgress' - The progress of the disk snapshot operation.
--
-- * 'dsName' - The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- * 'dsSizeInGb' - The size of the disk in GB.
--
-- * 'dsSupportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'dsFromInstanceARN' - The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
--
-- * 'dsFromInstanceName' - The unique name of the source instance from which the disk (system volume) snapshot was created.
--
-- * 'dsFromDiskARN' - The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
--
-- * 'dsTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
diskSnapshot ::
  DiskSnapshot
diskSnapshot =
  DiskSnapshot'
    { _dsFromDiskName = Nothing,
      _dsIsFromAutoSnapshot = Nothing,
      _dsState = Nothing,
      _dsResourceType = Nothing,
      _dsArn = Nothing,
      _dsCreatedAt = Nothing,
      _dsLocation = Nothing,
      _dsProgress = Nothing,
      _dsName = Nothing,
      _dsSizeInGb = Nothing,
      _dsSupportCode = Nothing,
      _dsFromInstanceARN = Nothing,
      _dsFromInstanceName = Nothing,
      _dsFromDiskARN = Nothing,
      _dsTags = Nothing
    }

-- | The unique name of the source disk from which the disk snapshot was created.
dsFromDiskName :: Lens' DiskSnapshot (Maybe Text)
dsFromDiskName = lens _dsFromDiskName (\s a -> s {_dsFromDiskName = a})

-- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
dsIsFromAutoSnapshot :: Lens' DiskSnapshot (Maybe Bool)
dsIsFromAutoSnapshot = lens _dsIsFromAutoSnapshot (\s a -> s {_dsIsFromAutoSnapshot = a})

-- | The status of the disk snapshot operation.
dsState :: Lens' DiskSnapshot (Maybe DiskSnapshotState)
dsState = lens _dsState (\s a -> s {_dsState = a})

-- | The Lightsail resource type (e.g., @DiskSnapshot@ ).
dsResourceType :: Lens' DiskSnapshot (Maybe ResourceType)
dsResourceType = lens _dsResourceType (\s a -> s {_dsResourceType = a})

-- | The Amazon Resource Name (ARN) of the disk snapshot.
dsArn :: Lens' DiskSnapshot (Maybe Text)
dsArn = lens _dsArn (\s a -> s {_dsArn = a})

-- | The date when the disk snapshot was created.
dsCreatedAt :: Lens' DiskSnapshot (Maybe UTCTime)
dsCreatedAt = lens _dsCreatedAt (\s a -> s {_dsCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where the disk snapshot was created.
dsLocation :: Lens' DiskSnapshot (Maybe ResourceLocation)
dsLocation = lens _dsLocation (\s a -> s {_dsLocation = a})

-- | The progress of the disk snapshot operation.
dsProgress :: Lens' DiskSnapshot (Maybe Text)
dsProgress = lens _dsProgress (\s a -> s {_dsProgress = a})

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
dsName :: Lens' DiskSnapshot (Maybe Text)
dsName = lens _dsName (\s a -> s {_dsName = a})

-- | The size of the disk in GB.
dsSizeInGb :: Lens' DiskSnapshot (Maybe Int)
dsSizeInGb = lens _dsSizeInGb (\s a -> s {_dsSizeInGb = a})

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
dsSupportCode :: Lens' DiskSnapshot (Maybe Text)
dsSupportCode = lens _dsSupportCode (\s a -> s {_dsSupportCode = a})

-- | The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
dsFromInstanceARN :: Lens' DiskSnapshot (Maybe Text)
dsFromInstanceARN = lens _dsFromInstanceARN (\s a -> s {_dsFromInstanceARN = a})

-- | The unique name of the source instance from which the disk (system volume) snapshot was created.
dsFromInstanceName :: Lens' DiskSnapshot (Maybe Text)
dsFromInstanceName = lens _dsFromInstanceName (\s a -> s {_dsFromInstanceName = a})

-- | The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
dsFromDiskARN :: Lens' DiskSnapshot (Maybe Text)
dsFromDiskARN = lens _dsFromDiskARN (\s a -> s {_dsFromDiskARN = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
dsTags :: Lens' DiskSnapshot [Tag]
dsTags = lens _dsTags (\s a -> s {_dsTags = a}) . _Default . _Coerce

instance FromJSON DiskSnapshot where
  parseJSON =
    withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            <$> (x .:? "fromDiskName")
            <*> (x .:? "isFromAutoSnapshot")
            <*> (x .:? "state")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "progress")
            <*> (x .:? "name")
            <*> (x .:? "sizeInGb")
            <*> (x .:? "supportCode")
            <*> (x .:? "fromInstanceArn")
            <*> (x .:? "fromInstanceName")
            <*> (x .:? "fromDiskArn")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable DiskSnapshot

instance NFData DiskSnapshot
