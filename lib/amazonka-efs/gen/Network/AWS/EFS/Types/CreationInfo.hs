{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.CreationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.CreationInfo
  ( CreationInfo (..)
  -- * Smart constructor
  , mkCreationInfo
  -- * Lenses
  , ciOwnerUid
  , ciOwnerGid
  , ciPermissions
  ) where

import qualified Network.AWS.EFS.Types.Permissions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Required if the @RootDirectory@ > @Path@ specified does not exist. Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ > @Path@ . If the access point root directory does not exist, EFS creates it with these settings when a client connects to the access point. When specifying @CreationInfo@ , you must include values for all properties. 
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ does not exist, attempts to mount the file system using the access point will fail.
--
-- /See:/ 'mkCreationInfo' smart constructor.
data CreationInfo = CreationInfo'
  { ownerUid :: Core.Natural
    -- ^ Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
  , ownerGid :: Core.Natural
    -- ^ Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
  , permissions :: Types.Permissions
    -- ^ Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreationInfo' value with any optional fields omitted.
mkCreationInfo
    :: Core.Natural -- ^ 'ownerUid'
    -> Core.Natural -- ^ 'ownerGid'
    -> Types.Permissions -- ^ 'permissions'
    -> CreationInfo
mkCreationInfo ownerUid ownerGid permissions
  = CreationInfo'{ownerUid, ownerGid, permissions}

-- | Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
--
-- /Note:/ Consider using 'ownerUid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOwnerUid :: Lens.Lens' CreationInfo Core.Natural
ciOwnerUid = Lens.field @"ownerUid"
{-# INLINEABLE ciOwnerUid #-}
{-# DEPRECATED ownerUid "Use generic-lens or generic-optics with 'ownerUid' instead"  #-}

-- | Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
--
-- /Note:/ Consider using 'ownerGid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOwnerGid :: Lens.Lens' CreationInfo Core.Natural
ciOwnerGid = Lens.field @"ownerGid"
{-# INLINEABLE ciOwnerGid #-}
{-# DEPRECATED ownerGid "Use generic-lens or generic-optics with 'ownerGid' instead"  #-}

-- | Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPermissions :: Lens.Lens' CreationInfo Types.Permissions
ciPermissions = Lens.field @"permissions"
{-# INLINEABLE ciPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

instance Core.FromJSON CreationInfo where
        toJSON CreationInfo{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OwnerUid" Core..= ownerUid),
                  Core.Just ("OwnerGid" Core..= ownerGid),
                  Core.Just ("Permissions" Core..= permissions)])

instance Core.FromJSON CreationInfo where
        parseJSON
          = Core.withObject "CreationInfo" Core.$
              \ x ->
                CreationInfo' Core.<$>
                  (x Core..: "OwnerUid") Core.<*> x Core..: "OwnerGid" Core.<*>
                    x Core..: "Permissions"
