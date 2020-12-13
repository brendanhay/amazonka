{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.CreationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.CreationInfo
  ( CreationInfo (..),

    -- * Smart constructor
    mkCreationInfo,

    -- * Lenses
    ciOwnerGid,
    ciPermissions,
    ciOwnerUid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Required if the @RootDirectory@ > @Path@ specified does not exist. Specifies the POSIX IDs and permissions to apply to the access point's @RootDirectory@ > @Path@ . If the access point root directory does not exist, EFS creates it with these settings when a client connects to the access point. When specifying @CreationInfo@ , you must include values for all properties.
--
-- /Important:/ If you do not provide @CreationInfo@ and the specified @RootDirectory@ does not exist, attempts to mount the file system using the access point will fail.
--
-- /See:/ 'mkCreationInfo' smart constructor.
data CreationInfo = CreationInfo'
  { -- | Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
    ownerGid :: Lude.Natural,
    -- | Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
    permissions :: Lude.Text,
    -- | Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
    ownerUid :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreationInfo' with the minimum fields required to make a request.
--
-- * 'ownerGid' - Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
-- * 'permissions' - Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
-- * 'ownerUid' - Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
mkCreationInfo ::
  -- | 'ownerGid'
  Lude.Natural ->
  -- | 'permissions'
  Lude.Text ->
  -- | 'ownerUid'
  Lude.Natural ->
  CreationInfo
mkCreationInfo pOwnerGid_ pPermissions_ pOwnerUid_ =
  CreationInfo'
    { ownerGid = pOwnerGid_,
      permissions = pPermissions_,
      ownerUid = pOwnerUid_
    }

-- | Specifies the POSIX group ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
--
-- /Note:/ Consider using 'ownerGid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOwnerGid :: Lens.Lens' CreationInfo Lude.Natural
ciOwnerGid = Lens.lens (ownerGid :: CreationInfo -> Lude.Natural) (\s a -> s {ownerGid = a} :: CreationInfo)
{-# DEPRECATED ciOwnerGid "Use generic-lens or generic-optics with 'ownerGid' instead." #-}

-- | Specifies the POSIX permissions to apply to the @RootDirectory@ , in the format of an octal number representing the file's mode bits.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPermissions :: Lens.Lens' CreationInfo Lude.Text
ciPermissions = Lens.lens (permissions :: CreationInfo -> Lude.Text) (\s a -> s {permissions = a} :: CreationInfo)
{-# DEPRECATED ciPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | Specifies the POSIX user ID to apply to the @RootDirectory@ . Accepts values from 0 to 2^32 (4294967295).
--
-- /Note:/ Consider using 'ownerUid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOwnerUid :: Lens.Lens' CreationInfo Lude.Natural
ciOwnerUid = Lens.lens (ownerUid :: CreationInfo -> Lude.Natural) (\s a -> s {ownerUid = a} :: CreationInfo)
{-# DEPRECATED ciOwnerUid "Use generic-lens or generic-optics with 'ownerUid' instead." #-}

instance Lude.FromJSON CreationInfo where
  parseJSON =
    Lude.withObject
      "CreationInfo"
      ( \x ->
          CreationInfo'
            Lude.<$> (x Lude..: "OwnerGid")
            Lude.<*> (x Lude..: "Permissions")
            Lude.<*> (x Lude..: "OwnerUid")
      )

instance Lude.ToJSON CreationInfo where
  toJSON CreationInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OwnerGid" Lude..= ownerGid),
            Lude.Just ("Permissions" Lude..= permissions),
            Lude.Just ("OwnerUid" Lude..= ownerUid)
          ]
      )
