{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.PosixUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.PosixUser where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The full POSIX identity, including the user ID, group ID, and any
-- secondary group IDs, on the access point that is used for all file
-- system operations performed by NFS clients using the access point.
--
-- /See:/ 'newPosixUser' smart constructor.
data PosixUser = PosixUser'
  { -- | Secondary POSIX group IDs used for all file system operations using this
    -- access point.
    secondaryGids :: Prelude.Maybe [Prelude.Natural],
    -- | The POSIX user ID used for all file system operations using this access
    -- point.
    uid :: Prelude.Natural,
    -- | The POSIX group ID used for all file system operations using this access
    -- point.
    gid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PosixUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secondaryGids', 'posixUser_secondaryGids' - Secondary POSIX group IDs used for all file system operations using this
-- access point.
--
-- 'uid', 'posixUser_uid' - The POSIX user ID used for all file system operations using this access
-- point.
--
-- 'gid', 'posixUser_gid' - The POSIX group ID used for all file system operations using this access
-- point.
newPosixUser ::
  -- | 'uid'
  Prelude.Natural ->
  -- | 'gid'
  Prelude.Natural ->
  PosixUser
newPosixUser pUid_ pGid_ =
  PosixUser'
    { secondaryGids = Prelude.Nothing,
      uid = pUid_,
      gid = pGid_
    }

-- | Secondary POSIX group IDs used for all file system operations using this
-- access point.
posixUser_secondaryGids :: Lens.Lens' PosixUser (Prelude.Maybe [Prelude.Natural])
posixUser_secondaryGids = Lens.lens (\PosixUser' {secondaryGids} -> secondaryGids) (\s@PosixUser' {} a -> s {secondaryGids = a} :: PosixUser) Prelude.. Lens.mapping Lens._Coerce

-- | The POSIX user ID used for all file system operations using this access
-- point.
posixUser_uid :: Lens.Lens' PosixUser Prelude.Natural
posixUser_uid = Lens.lens (\PosixUser' {uid} -> uid) (\s@PosixUser' {} a -> s {uid = a} :: PosixUser)

-- | The POSIX group ID used for all file system operations using this access
-- point.
posixUser_gid :: Lens.Lens' PosixUser Prelude.Natural
posixUser_gid = Lens.lens (\PosixUser' {gid} -> gid) (\s@PosixUser' {} a -> s {gid = a} :: PosixUser)

instance Core.FromJSON PosixUser where
  parseJSON =
    Core.withObject
      "PosixUser"
      ( \x ->
          PosixUser'
            Prelude.<$> (x Core..:? "SecondaryGids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Uid")
            Prelude.<*> (x Core..: "Gid")
      )

instance Prelude.Hashable PosixUser

instance Prelude.NFData PosixUser

instance Core.ToJSON PosixUser where
  toJSON PosixUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecondaryGids" Core..=) Prelude.<$> secondaryGids,
            Prelude.Just ("Uid" Core..= uid),
            Prelude.Just ("Gid" Core..= gid)
          ]
      )
