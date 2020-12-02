{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Disk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Disk where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a gateway's local disk.
--
--
--
-- /See:/ 'disk' smart constructor.
data Disk = Disk'
  { _dDiskAllocationResource :: !(Maybe Text),
    _dDiskAllocationType :: !(Maybe Text),
    _dDiskNode :: !(Maybe Text),
    _dDiskPath :: !(Maybe Text),
    _dDiskSizeInBytes :: !(Maybe Integer),
    _dDiskStatus :: !(Maybe Text),
    _dDiskId :: !(Maybe Text),
    _dDiskAttributeList :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDiskAllocationResource' - The iSCSI qualified name (IQN) that is defined for a disk. This field is not included in the response if the local disk is not defined as an iSCSI target. The format of this field is /targetIqn::LUNNumber::region-volumeId/ .
--
-- * 'dDiskAllocationType' - Undocumented member.
--
-- * 'dDiskNode' - The device node of a local disk as assigned by the virtualization environment.
--
-- * 'dDiskPath' - The path of a local disk in the gateway virtual machine (VM).
--
-- * 'dDiskSizeInBytes' - The local disk size in bytes.
--
-- * 'dDiskStatus' - A value that represents the status of a local disk.
--
-- * 'dDiskId' - The unique device ID or other distinguishing data that identifies a local disk.
--
-- * 'dDiskAttributeList' - Undocumented member.
disk ::
  Disk
disk =
  Disk'
    { _dDiskAllocationResource = Nothing,
      _dDiskAllocationType = Nothing,
      _dDiskNode = Nothing,
      _dDiskPath = Nothing,
      _dDiskSizeInBytes = Nothing,
      _dDiskStatus = Nothing,
      _dDiskId = Nothing,
      _dDiskAttributeList = Nothing
    }

-- | The iSCSI qualified name (IQN) that is defined for a disk. This field is not included in the response if the local disk is not defined as an iSCSI target. The format of this field is /targetIqn::LUNNumber::region-volumeId/ .
dDiskAllocationResource :: Lens' Disk (Maybe Text)
dDiskAllocationResource = lens _dDiskAllocationResource (\s a -> s {_dDiskAllocationResource = a})

-- | Undocumented member.
dDiskAllocationType :: Lens' Disk (Maybe Text)
dDiskAllocationType = lens _dDiskAllocationType (\s a -> s {_dDiskAllocationType = a})

-- | The device node of a local disk as assigned by the virtualization environment.
dDiskNode :: Lens' Disk (Maybe Text)
dDiskNode = lens _dDiskNode (\s a -> s {_dDiskNode = a})

-- | The path of a local disk in the gateway virtual machine (VM).
dDiskPath :: Lens' Disk (Maybe Text)
dDiskPath = lens _dDiskPath (\s a -> s {_dDiskPath = a})

-- | The local disk size in bytes.
dDiskSizeInBytes :: Lens' Disk (Maybe Integer)
dDiskSizeInBytes = lens _dDiskSizeInBytes (\s a -> s {_dDiskSizeInBytes = a})

-- | A value that represents the status of a local disk.
dDiskStatus :: Lens' Disk (Maybe Text)
dDiskStatus = lens _dDiskStatus (\s a -> s {_dDiskStatus = a})

-- | The unique device ID or other distinguishing data that identifies a local disk.
dDiskId :: Lens' Disk (Maybe Text)
dDiskId = lens _dDiskId (\s a -> s {_dDiskId = a})

-- | Undocumented member.
dDiskAttributeList :: Lens' Disk [Text]
dDiskAttributeList = lens _dDiskAttributeList (\s a -> s {_dDiskAttributeList = a}) . _Default . _Coerce

instance FromJSON Disk where
  parseJSON =
    withObject
      "Disk"
      ( \x ->
          Disk'
            <$> (x .:? "DiskAllocationResource")
            <*> (x .:? "DiskAllocationType")
            <*> (x .:? "DiskNode")
            <*> (x .:? "DiskPath")
            <*> (x .:? "DiskSizeInBytes")
            <*> (x .:? "DiskStatus")
            <*> (x .:? "DiskId")
            <*> (x .:? "DiskAttributeList" .!= mempty)
      )

instance Hashable Disk

instance NFData Disk
