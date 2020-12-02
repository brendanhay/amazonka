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
-- Module      : Network.AWS.EFS.UpdateFileSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the throughput mode or the amount of provisioned throughput of an existing file system.
module Network.AWS.EFS.UpdateFileSystem
  ( -- * Creating a Request
    updateFileSystem,
    UpdateFileSystem,

    -- * Request Lenses
    ufsProvisionedThroughputInMibps,
    ufsThroughputMode,
    ufsFileSystemId,

    -- * Destructuring the Response
    fileSystemDescription,
    FileSystemDescription,

    -- * Response Lenses
    fsdProvisionedThroughputInMibps,
    fsdFileSystemARN,
    fsdEncrypted,
    fsdThroughputMode,
    fsdKMSKeyId,
    fsdName,
    fsdOwnerId,
    fsdCreationToken,
    fsdFileSystemId,
    fsdCreationTime,
    fsdLifeCycleState,
    fsdNumberOfMountTargets,
    fsdSizeInBytes,
    fsdPerformanceMode,
    fsdTags,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFileSystem' smart constructor.
data UpdateFileSystem = UpdateFileSystem'
  { _ufsProvisionedThroughputInMibps ::
      !(Maybe Double),
    _ufsThroughputMode :: !(Maybe ThroughputMode),
    _ufsFileSystemId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFileSystem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufsProvisionedThroughputInMibps' - (Optional) The amount of throughput, in MiB/s, that you want to provision for your file system. Valid values are 1-1024. Required if @ThroughputMode@ is changed to @provisioned@ on update. If you're not updating the amount of provisioned throughput for your file system, you don't need to provide this value in your request.
--
-- * 'ufsThroughputMode' - (Optional) The throughput mode that you want your file system to use. If you're not updating your throughput mode, you don't need to provide this value in your request. If you are changing the @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughputInMibps@ .
--
-- * 'ufsFileSystemId' - The ID of the file system that you want to update.
updateFileSystem ::
  -- | 'ufsFileSystemId'
  Text ->
  UpdateFileSystem
updateFileSystem pFileSystemId_ =
  UpdateFileSystem'
    { _ufsProvisionedThroughputInMibps = Nothing,
      _ufsThroughputMode = Nothing,
      _ufsFileSystemId = pFileSystemId_
    }

-- | (Optional) The amount of throughput, in MiB/s, that you want to provision for your file system. Valid values are 1-1024. Required if @ThroughputMode@ is changed to @provisioned@ on update. If you're not updating the amount of provisioned throughput for your file system, you don't need to provide this value in your request.
ufsProvisionedThroughputInMibps :: Lens' UpdateFileSystem (Maybe Double)
ufsProvisionedThroughputInMibps = lens _ufsProvisionedThroughputInMibps (\s a -> s {_ufsProvisionedThroughputInMibps = a})

-- | (Optional) The throughput mode that you want your file system to use. If you're not updating your throughput mode, you don't need to provide this value in your request. If you are changing the @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughputInMibps@ .
ufsThroughputMode :: Lens' UpdateFileSystem (Maybe ThroughputMode)
ufsThroughputMode = lens _ufsThroughputMode (\s a -> s {_ufsThroughputMode = a})

-- | The ID of the file system that you want to update.
ufsFileSystemId :: Lens' UpdateFileSystem Text
ufsFileSystemId = lens _ufsFileSystemId (\s a -> s {_ufsFileSystemId = a})

instance AWSRequest UpdateFileSystem where
  type Rs UpdateFileSystem = FileSystemDescription
  request = putJSON efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable UpdateFileSystem

instance NFData UpdateFileSystem

instance ToHeaders UpdateFileSystem where
  toHeaders = const mempty

instance ToJSON UpdateFileSystem where
  toJSON UpdateFileSystem' {..} =
    object
      ( catMaybes
          [ ("ProvisionedThroughputInMibps" .=)
              <$> _ufsProvisionedThroughputInMibps,
            ("ThroughputMode" .=) <$> _ufsThroughputMode
          ]
      )

instance ToPath UpdateFileSystem where
  toPath UpdateFileSystem' {..} =
    mconcat ["/2015-02-01/file-systems/", toBS _ufsFileSystemId]

instance ToQuery UpdateFileSystem where
  toQuery = const mempty
