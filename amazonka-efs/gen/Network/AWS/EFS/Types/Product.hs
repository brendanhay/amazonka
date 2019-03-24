{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.Product where

import Network.AWS.EFS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of the file system.
--
--
--
-- /See:/ 'fileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { _fsdProvisionedThroughputInMibps :: !(Maybe Double)
  , _fsdEncrypted                    :: !(Maybe Bool)
  , _fsdThroughputMode               :: !(Maybe ThroughputMode)
  , _fsdKMSKeyId                     :: !(Maybe Text)
  , _fsdName                         :: !(Maybe Text)
  , _fsdOwnerId                      :: !Text
  , _fsdCreationToken                :: !Text
  , _fsdFileSystemId                 :: !Text
  , _fsdCreationTime                 :: !POSIX
  , _fsdLifeCycleState               :: !LifeCycleState
  , _fsdNumberOfMountTargets         :: !Nat
  , _fsdSizeInBytes                  :: !FileSystemSize
  , _fsdPerformanceMode              :: !PerformanceMode
  , _fsdTags                         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileSystemDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsdProvisionedThroughputInMibps' - The throughput, measured in MiB/s, that you want to provision for a file system. The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
--
-- * 'fsdEncrypted' - A Boolean value that, if true, indicates that the file system is encrypted.
--
-- * 'fsdThroughputMode' - The throughput mode for a file system. There are two throughput modes to choose from for your file system: bursting and provisioned. You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it
