{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.RootDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.RootDirectory
  ( RootDirectory (..)
  -- * Smart constructor
  , mkRootDirectory
  -- * Lenses
  , rdCreationInfo
  , rdPath
  ) where

import qualified Network.AWS.EFS.Types.CreationInfo as Types
import qualified Network.AWS.EFS.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the directory on the Amazon EFS file system that the access point provides access to. The access point exposes the specified file system path as the root directory of your file system to applications using the access point. NFS clients using the access point can only access data in the access point's @RootDirectory@ and it's subdirectories.
--
-- /See:/ 'mkRootDirectory' smart constructor.
data RootDirectory = RootDirectory'
  { creationInfo :: Core.Maybe Types.CreationInfo
    -- ^ (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties. 
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
  , path :: Core.Maybe Types.Path
    -- ^ Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RootDirectory' value with any optional fields omitted.
mkRootDirectory
    :: RootDirectory
mkRootDirectory
  = RootDirectory'{creationInfo = Core.Nothing, path = Core.Nothing}

-- | (Optional) Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ . If the @RootDirectory@ > @Path@ specified does not exist, EFS creates the root directory using the @CreationInfo@ settings when a client connects to an access point. When specifying the @CreationInfo@ , you must provide values for all properties. 
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ > @Path@ does not exist, attempts to mount the file system using the access point will fail.
--
-- /Note:/ Consider using 'creationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCreationInfo :: Lens.Lens' RootDirectory (Core.Maybe Types.CreationInfo)
rdCreationInfo = Lens.field @"creationInfo"
{-# INLINEABLE rdCreationInfo #-}
{-# DEPRECATED creationInfo "Use generic-lens or generic-optics with 'creationInfo' instead"  #-}

-- | Specifies the path on the EFS file system to expose as the root directory to NFS clients using the access point to access the EFS file system. A path can have up to four subdirectories. If the specified path does not exist, you are required to provide the @CreationInfo@ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdPath :: Lens.Lens' RootDirectory (Core.Maybe Types.Path)
rdPath = Lens.field @"path"
{-# INLINEABLE rdPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.FromJSON RootDirectory where
        toJSON RootDirectory{..}
          = Core.object
              (Core.catMaybes
                 [("CreationInfo" Core..=) Core.<$> creationInfo,
                  ("Path" Core..=) Core.<$> path])

instance Core.FromJSON RootDirectory where
        parseJSON
          = Core.withObject "RootDirectory" Core.$
              \ x ->
                RootDirectory' Core.<$>
                  (x Core..:? "CreationInfo") Core.<*> x Core..:? "Path"
