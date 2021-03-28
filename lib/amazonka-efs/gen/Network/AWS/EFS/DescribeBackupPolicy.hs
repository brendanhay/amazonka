{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup policy for the specified EFS file system.
module Network.AWS.EFS.DescribeBackupPolicy
    (
    -- * Creating a request
      DescribeBackupPolicy (..)
    , mkDescribeBackupPolicy
    -- ** Request lenses
    , dbpFileSystemId

     -- * Destructuring the response
    , Types.BackupPolicyDescription (..)
    , Types.mkBackupPolicyDescription
    -- ** Response lenses
    , Types.bpdBackupPolicy
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBackupPolicy' smart constructor.
newtype DescribeBackupPolicy = DescribeBackupPolicy'
  { fileSystemId :: Types.FileSystemId
    -- ^ Specifies which EFS file system to retrieve the @BackupPolicy@ for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBackupPolicy' value with any optional fields omitted.
mkDescribeBackupPolicy
    :: Types.FileSystemId -- ^ 'fileSystemId'
    -> DescribeBackupPolicy
mkDescribeBackupPolicy fileSystemId
  = DescribeBackupPolicy'{fileSystemId}

-- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpFileSystemId :: Lens.Lens' DescribeBackupPolicy Types.FileSystemId
dbpFileSystemId = Lens.field @"fileSystemId"
{-# INLINEABLE dbpFileSystemId #-}
{-# DEPRECATED fileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead"  #-}

instance Core.ToQuery DescribeBackupPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBackupPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeBackupPolicy where
        type Rs DescribeBackupPolicy = Types.BackupPolicyDescription
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-02-01/file-systems/" Core.<> Core.toText fileSystemId
                             Core.<> "/backup-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
