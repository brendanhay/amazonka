{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateFileSystem (..)
    , mkUpdateFileSystem
    -- ** Request lenses
    , ufsFileSystemId
    , ufsProvisionedThroughputInMibps
    , ufsThroughputMode

     -- * Destructuring the response
    , Types.FileSystemDescription (..)
    , Types.mkFileSystemDescription
    -- ** Response lenses
    , Types.fsdOwnerId
    , Types.fsdCreationToken
    , Types.fsdFileSystemId
    , Types.fsdCreationTime
    , Types.fsdLifeCycleState
    , Types.fsdNumberOfMountTargets
    , Types.fsdSizeInBytes
    , Types.fsdPerformanceMode
    , Types.fsdTags
    , Types.fsdEncrypted
    , Types.fsdFileSystemArn
    , Types.fsdKmsKeyId
    , Types.fsdName
    , Types.fsdProvisionedThroughputInMibps
    , Types.fsdThroughputMode
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFileSystem' smart constructor.
data UpdateFileSystem = UpdateFileSystem'
  { fileSystemId :: Types.FileSystemId
    -- ^ The ID of the file system that you want to update.
  , provisionedThroughputInMibps :: Core.Maybe Core.Double
    -- ^ (Optional) The amount of throughput, in MiB/s, that you want to provision for your file system. Valid values are 1-1024. Required if @ThroughputMode@ is changed to @provisioned@ on update. If you're not updating the amount of provisioned throughput for your file system, you don't need to provide this value in your request. 
  , throughputMode :: Core.Maybe Types.ThroughputMode
    -- ^ (Optional) The throughput mode that you want your file system to use. If you're not updating your throughput mode, you don't need to provide this value in your request. If you are changing the @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughputInMibps@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFileSystem' value with any optional fields omitted.
mkUpdateFileSystem
    :: Types.FileSystemId -- ^ 'fileSystemId'
    -> UpdateFileSystem
mkUpdateFileSystem fileSystemId
  = UpdateFileSystem'{fileSystemId,
                      provisionedThroughputInMibps = Core.Nothing,
                      throughputMode = Core.Nothing}

-- | The ID of the file system that you want to update.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufsFileSystemId :: Lens.Lens' UpdateFileSystem Types.FileSystemId
ufsFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE ufsFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

-- | (Optional) The amount of throughput, in MiB/s, that you want to provision for your file system. Valid values are 1-1024. Required if @ThroughputMode@ is changed to @provisioned@ on update. If you're not updating the amount of provisioned throughput for your file system, you don't need to provide this value in your request. 
--
-- /Note:/ Consider using 'provisionedThroughputInMibps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufsProvisionedThroughputInMibps :: Lens.Lens' UpdateFileSystem (Core.Maybe Core.Double)
ufsProvisionedThroughputInMibps = Lens.field @"provisionedThroughputInMibps"
{-# INLINEABLE ufsProvisionedThroughputInMibps #-}
{-# DEPRECATED provisionedThroughputInMibps "Use generic-lens or generic-optics with 'provisionedThroughputInMibps' instead"  #-}

-- | (Optional) The throughput mode that you want your file system to use. If you're not updating your throughput mode, you don't need to provide this value in your request. If you are changing the @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughputInMibps@ .
--
-- /Note:/ Consider using 'throughputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufsThroughputMode :: Lens.Lens' UpdateFileSystem (Core.Maybe Types.ThroughputMode)
ufsThroughputMode = Lens.field @"throughputMode"
{-# INLINEABLE ufsThroughputMode #-}
{-# DEPRECATED throughputMode "Use generic-lens or generic-optics with 'throughputMode' instead"  #-}

instance Core.ToQuery UpdateFileSystem where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFileSystem where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateFileSystem where
        toJSON UpdateFileSystem{..}
          = Core.object
              (Core.catMaybes
                 [("ProvisionedThroughputInMibps" Core..=) Core.<$>
                    provisionedThroughputInMibps,
                  ("ThroughputMode" Core..=) Core.<$> throughputMode])

instance Core.AWSRequest UpdateFileSystem where
        type Rs UpdateFileSystem = Types.FileSystemDescription
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2015-02-01/file-systems/" Core.<> Core.toText fileSystemId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
