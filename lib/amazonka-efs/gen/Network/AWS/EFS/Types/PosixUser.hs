{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.PosixUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.PosixUser
  ( PosixUser (..),

    -- * Smart constructor
    mkPosixUser,

    -- * Lenses
    puSecondaryGids,
    puUid,
    puGid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The full POSIX identity, including the user ID, group ID, and any secondary group IDs, on the access point that is used for all file system operations performed by NFS clients using the access point.
--
-- /See:/ 'mkPosixUser' smart constructor.
data PosixUser = PosixUser'
  { secondaryGids ::
      Lude.Maybe [Lude.Natural],
    uid :: Lude.Natural,
    gid :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PosixUser' with the minimum fields required to make a request.
--
-- * 'gid' - The POSIX group ID used for all file system operations using this access point.
-- * 'secondaryGids' - Secondary POSIX group IDs used for all file system operations using this access point.
-- * 'uid' - The POSIX user ID used for all file system operations using this access point.
mkPosixUser ::
  -- | 'uid'
  Lude.Natural ->
  -- | 'gid'
  Lude.Natural ->
  PosixUser
mkPosixUser pUid_ pGid_ =
  PosixUser'
    { secondaryGids = Lude.Nothing,
      uid = pUid_,
      gid = pGid_
    }

-- | Secondary POSIX group IDs used for all file system operations using this access point.
--
-- /Note:/ Consider using 'secondaryGids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puSecondaryGids :: Lens.Lens' PosixUser (Lude.Maybe [Lude.Natural])
puSecondaryGids = Lens.lens (secondaryGids :: PosixUser -> Lude.Maybe [Lude.Natural]) (\s a -> s {secondaryGids = a} :: PosixUser)
{-# DEPRECATED puSecondaryGids "Use generic-lens or generic-optics with 'secondaryGids' instead." #-}

-- | The POSIX user ID used for all file system operations using this access point.
--
-- /Note:/ Consider using 'uid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puUid :: Lens.Lens' PosixUser Lude.Natural
puUid = Lens.lens (uid :: PosixUser -> Lude.Natural) (\s a -> s {uid = a} :: PosixUser)
{-# DEPRECATED puUid "Use generic-lens or generic-optics with 'uid' instead." #-}

-- | The POSIX group ID used for all file system operations using this access point.
--
-- /Note:/ Consider using 'gid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puGid :: Lens.Lens' PosixUser Lude.Natural
puGid = Lens.lens (gid :: PosixUser -> Lude.Natural) (\s a -> s {gid = a} :: PosixUser)
{-# DEPRECATED puGid "Use generic-lens or generic-optics with 'gid' instead." #-}

instance Lude.FromJSON PosixUser where
  parseJSON =
    Lude.withObject
      "PosixUser"
      ( \x ->
          PosixUser'
            Lude.<$> (x Lude..:? "SecondaryGids" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Uid")
            Lude.<*> (x Lude..: "Gid")
      )

instance Lude.ToJSON PosixUser where
  toJSON PosixUser' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecondaryGids" Lude..=) Lude.<$> secondaryGids,
            Lude.Just ("Uid" Lude..= uid),
            Lude.Just ("Gid" Lude..= gid)
          ]
      )
