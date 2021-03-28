{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.PosixUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.PosixUser
  ( PosixUser (..)
  -- * Smart constructor
  , mkPosixUser
  -- * Lenses
  , puUid
  , puGid
  , puSecondaryGids
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The full POSIX identity, including the user ID, group ID, and any secondary group IDs, on the access point that is used for all file system operations performed by NFS clients using the access point.
--
-- /See:/ 'mkPosixUser' smart constructor.
data PosixUser = PosixUser'
  { uid :: Core.Natural
    -- ^ The POSIX user ID used for all file system operations using this access point.
  , gid :: Core.Natural
    -- ^ The POSIX group ID used for all file system operations using this access point.
  , secondaryGids :: Core.Maybe [Core.Natural]
    -- ^ Secondary POSIX group IDs used for all file system operations using this access point.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PosixUser' value with any optional fields omitted.
mkPosixUser
    :: Core.Natural -- ^ 'uid'
    -> Core.Natural -- ^ 'gid'
    -> PosixUser
mkPosixUser uid gid
  = PosixUser'{uid, gid, secondaryGids = Core.Nothing}

-- | The POSIX user ID used for all file system operations using this access point.
--
-- /Note:/ Consider using 'uid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puUid :: Lens.Lens' PosixUser Core.Natural
puUid = Lens.field @"uid"
{-# INLINEABLE puUid #-}
{-# DEPRECATED uid "Use generic-lens or generic-optics with 'uid' instead"  #-}

-- | The POSIX group ID used for all file system operations using this access point.
--
-- /Note:/ Consider using 'gid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puGid :: Lens.Lens' PosixUser Core.Natural
puGid = Lens.field @"gid"
{-# INLINEABLE puGid #-}
{-# DEPRECATED gid "Use generic-lens or generic-optics with 'gid' instead"  #-}

-- | Secondary POSIX group IDs used for all file system operations using this access point.
--
-- /Note:/ Consider using 'secondaryGids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puSecondaryGids :: Lens.Lens' PosixUser (Core.Maybe [Core.Natural])
puSecondaryGids = Lens.field @"secondaryGids"
{-# INLINEABLE puSecondaryGids #-}
{-# DEPRECATED secondaryGids "Use generic-lens or generic-optics with 'secondaryGids' instead"  #-}

instance Core.FromJSON PosixUser where
        toJSON PosixUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Uid" Core..= uid), Core.Just ("Gid" Core..= gid),
                  ("SecondaryGids" Core..=) Core.<$> secondaryGids])

instance Core.FromJSON PosixUser where
        parseJSON
          = Core.withObject "PosixUser" Core.$
              \ x ->
                PosixUser' Core.<$>
                  (x Core..: "Uid") Core.<*> x Core..: "Gid" Core.<*>
                    x Core..:? "SecondaryGids"
