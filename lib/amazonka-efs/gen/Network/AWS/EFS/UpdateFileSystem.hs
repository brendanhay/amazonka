{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    UpdateFileSystem (..),
    mkUpdateFileSystem,

    -- ** Request lenses
    ufsProvisionedThroughputInMibps,
    ufsThroughputMode,
    ufsFileSystemId,

    -- * Destructuring the response
    FileSystemDescription (..),
    mkFileSystemDescription,

    -- ** Response lenses
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFileSystem' smart constructor.
data UpdateFileSystem = UpdateFileSystem'
  { provisionedThroughputInMibps ::
      Lude.Maybe Lude.Double,
    throughputMode :: Lude.Maybe ThroughputMode,
    fileSystemId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFileSystem' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - The ID of the file system that you want to update.
-- * 'provisionedThroughputInMibps' - (Optional) The amount of throughput, in MiB/s, that you want to provision for your file system. Valid values are 1-1024. Required if @ThroughputMode@ is changed to @provisioned@ on update. If you're not updating the amount of provisioned throughput for your file system, you don't need to provide this value in your request.
-- * 'throughputMode' - (Optional) The throughput mode that you want your file system to use. If you're not updating your throughput mode, you don't need to provide this value in your request. If you are changing the @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughputInMibps@ .
mkUpdateFileSystem ::
  -- | 'fileSystemId'
  Lude.Text ->
  UpdateFileSystem
mkUpdateFileSystem pFileSystemId_ =
  UpdateFileSystem'
    { provisionedThroughputInMibps = Lude.Nothing,
      throughputMode = Lude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | (Optional) The amount of throughput, in MiB/s, that you want to provision for your file system. Valid values are 1-1024. Required if @ThroughputMode@ is changed to @provisioned@ on update. If you're not updating the amount of provisioned throughput for your file system, you don't need to provide this value in your request.
--
-- /Note:/ Consider using 'provisionedThroughputInMibps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufsProvisionedThroughputInMibps :: Lens.Lens' UpdateFileSystem (Lude.Maybe Lude.Double)
ufsProvisionedThroughputInMibps = Lens.lens (provisionedThroughputInMibps :: UpdateFileSystem -> Lude.Maybe Lude.Double) (\s a -> s {provisionedThroughputInMibps = a} :: UpdateFileSystem)
{-# DEPRECATED ufsProvisionedThroughputInMibps "Use generic-lens or generic-optics with 'provisionedThroughputInMibps' instead." #-}

-- | (Optional) The throughput mode that you want your file system to use. If you're not updating your throughput mode, you don't need to provide this value in your request. If you are changing the @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughputInMibps@ .
--
-- /Note:/ Consider using 'throughputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufsThroughputMode :: Lens.Lens' UpdateFileSystem (Lude.Maybe ThroughputMode)
ufsThroughputMode = Lens.lens (throughputMode :: UpdateFileSystem -> Lude.Maybe ThroughputMode) (\s a -> s {throughputMode = a} :: UpdateFileSystem)
{-# DEPRECATED ufsThroughputMode "Use generic-lens or generic-optics with 'throughputMode' instead." #-}

-- | The ID of the file system that you want to update.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufsFileSystemId :: Lens.Lens' UpdateFileSystem Lude.Text
ufsFileSystemId = Lens.lens (fileSystemId :: UpdateFileSystem -> Lude.Text) (\s a -> s {fileSystemId = a} :: UpdateFileSystem)
{-# DEPRECATED ufsFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest UpdateFileSystem where
  type Rs UpdateFileSystem = FileSystemDescription
  request = Req.putJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateFileSystem where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateFileSystem where
  toJSON UpdateFileSystem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedThroughputInMibps" Lude..=)
              Lude.<$> provisionedThroughputInMibps,
            ("ThroughputMode" Lude..=) Lude.<$> throughputMode
          ]
      )

instance Lude.ToPath UpdateFileSystem where
  toPath UpdateFileSystem' {..} =
    Lude.mconcat
      ["/2015-02-01/file-systems/", Lude.toBS fileSystemId]

instance Lude.ToQuery UpdateFileSystem where
  toQuery = Lude.const Lude.mempty
