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
-- Module      : Amazonka.Transfer.Types.PosixProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.PosixProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The full POSIX identity, including user ID (@Uid@), group ID (@Gid@),
-- and any secondary groups IDs (@SecondaryGids@), that controls your
-- users\' access to your Amazon EFS file systems. The POSIX permissions
-- that are set on files and directories in your file system determine the
-- level of access your users get when transferring files into and out of
-- your Amazon EFS file systems.
--
-- /See:/ 'newPosixProfile' smart constructor.
data PosixProfile = PosixProfile'
  { -- | The secondary POSIX group IDs used for all EFS operations by this user.
    secondaryGids :: Prelude.Maybe [Prelude.Natural],
    -- | The POSIX user ID used for all EFS operations by this user.
    uid :: Prelude.Natural,
    -- | The POSIX group ID used for all EFS operations by this user.
    gid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PosixProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secondaryGids', 'posixProfile_secondaryGids' - The secondary POSIX group IDs used for all EFS operations by this user.
--
-- 'uid', 'posixProfile_uid' - The POSIX user ID used for all EFS operations by this user.
--
-- 'gid', 'posixProfile_gid' - The POSIX group ID used for all EFS operations by this user.
newPosixProfile ::
  -- | 'uid'
  Prelude.Natural ->
  -- | 'gid'
  Prelude.Natural ->
  PosixProfile
newPosixProfile pUid_ pGid_ =
  PosixProfile'
    { secondaryGids = Prelude.Nothing,
      uid = pUid_,
      gid = pGid_
    }

-- | The secondary POSIX group IDs used for all EFS operations by this user.
posixProfile_secondaryGids :: Lens.Lens' PosixProfile (Prelude.Maybe [Prelude.Natural])
posixProfile_secondaryGids = Lens.lens (\PosixProfile' {secondaryGids} -> secondaryGids) (\s@PosixProfile' {} a -> s {secondaryGids = a} :: PosixProfile) Prelude.. Lens.mapping Lens.coerced

-- | The POSIX user ID used for all EFS operations by this user.
posixProfile_uid :: Lens.Lens' PosixProfile Prelude.Natural
posixProfile_uid = Lens.lens (\PosixProfile' {uid} -> uid) (\s@PosixProfile' {} a -> s {uid = a} :: PosixProfile)

-- | The POSIX group ID used for all EFS operations by this user.
posixProfile_gid :: Lens.Lens' PosixProfile Prelude.Natural
posixProfile_gid = Lens.lens (\PosixProfile' {gid} -> gid) (\s@PosixProfile' {} a -> s {gid = a} :: PosixProfile)

instance Data.FromJSON PosixProfile where
  parseJSON =
    Data.withObject
      "PosixProfile"
      ( \x ->
          PosixProfile'
            Prelude.<$> (x Data..:? "SecondaryGids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Uid")
            Prelude.<*> (x Data..: "Gid")
      )

instance Prelude.Hashable PosixProfile where
  hashWithSalt _salt PosixProfile' {..} =
    _salt
      `Prelude.hashWithSalt` secondaryGids
      `Prelude.hashWithSalt` uid
      `Prelude.hashWithSalt` gid

instance Prelude.NFData PosixProfile where
  rnf PosixProfile' {..} =
    Prelude.rnf secondaryGids `Prelude.seq`
      Prelude.rnf uid `Prelude.seq`
        Prelude.rnf gid

instance Data.ToJSON PosixProfile where
  toJSON PosixProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecondaryGids" Data..=) Prelude.<$> secondaryGids,
            Prelude.Just ("Uid" Data..= uid),
            Prelude.Just ("Gid" Data..= gid)
          ]
      )
