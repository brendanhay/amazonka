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
-- Module      : Amazonka.SecurityHub.Types.AwsEfsAccessPointPosixUserDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEfsAccessPointPosixUserDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details for all file system operations using this Amazon EFS
-- access point.
--
-- /See:/ 'newAwsEfsAccessPointPosixUserDetails' smart constructor.
data AwsEfsAccessPointPosixUserDetails = AwsEfsAccessPointPosixUserDetails'
  { -- | The POSIX group ID used for all file system operations using this access
    -- point.
    gid :: Prelude.Maybe Prelude.Text,
    -- | Secondary POSIX group IDs used for all file system operations using this
    -- access point.
    secondaryGids :: Prelude.Maybe [Prelude.Text],
    -- | The POSIX user ID used for all file system operations using this access
    -- point.
    uid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEfsAccessPointPosixUserDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gid', 'awsEfsAccessPointPosixUserDetails_gid' - The POSIX group ID used for all file system operations using this access
-- point.
--
-- 'secondaryGids', 'awsEfsAccessPointPosixUserDetails_secondaryGids' - Secondary POSIX group IDs used for all file system operations using this
-- access point.
--
-- 'uid', 'awsEfsAccessPointPosixUserDetails_uid' - The POSIX user ID used for all file system operations using this access
-- point.
newAwsEfsAccessPointPosixUserDetails ::
  AwsEfsAccessPointPosixUserDetails
newAwsEfsAccessPointPosixUserDetails =
  AwsEfsAccessPointPosixUserDetails'
    { gid =
        Prelude.Nothing,
      secondaryGids = Prelude.Nothing,
      uid = Prelude.Nothing
    }

-- | The POSIX group ID used for all file system operations using this access
-- point.
awsEfsAccessPointPosixUserDetails_gid :: Lens.Lens' AwsEfsAccessPointPosixUserDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointPosixUserDetails_gid = Lens.lens (\AwsEfsAccessPointPosixUserDetails' {gid} -> gid) (\s@AwsEfsAccessPointPosixUserDetails' {} a -> s {gid = a} :: AwsEfsAccessPointPosixUserDetails)

-- | Secondary POSIX group IDs used for all file system operations using this
-- access point.
awsEfsAccessPointPosixUserDetails_secondaryGids :: Lens.Lens' AwsEfsAccessPointPosixUserDetails (Prelude.Maybe [Prelude.Text])
awsEfsAccessPointPosixUserDetails_secondaryGids = Lens.lens (\AwsEfsAccessPointPosixUserDetails' {secondaryGids} -> secondaryGids) (\s@AwsEfsAccessPointPosixUserDetails' {} a -> s {secondaryGids = a} :: AwsEfsAccessPointPosixUserDetails) Prelude.. Lens.mapping Lens.coerced

-- | The POSIX user ID used for all file system operations using this access
-- point.
awsEfsAccessPointPosixUserDetails_uid :: Lens.Lens' AwsEfsAccessPointPosixUserDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointPosixUserDetails_uid = Lens.lens (\AwsEfsAccessPointPosixUserDetails' {uid} -> uid) (\s@AwsEfsAccessPointPosixUserDetails' {} a -> s {uid = a} :: AwsEfsAccessPointPosixUserDetails)

instance
  Core.FromJSON
    AwsEfsAccessPointPosixUserDetails
  where
  parseJSON =
    Core.withObject
      "AwsEfsAccessPointPosixUserDetails"
      ( \x ->
          AwsEfsAccessPointPosixUserDetails'
            Prelude.<$> (x Core..:? "Gid")
            Prelude.<*> (x Core..:? "SecondaryGids" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Uid")
      )

instance
  Prelude.Hashable
    AwsEfsAccessPointPosixUserDetails
  where
  hashWithSalt
    _salt
    AwsEfsAccessPointPosixUserDetails' {..} =
      _salt `Prelude.hashWithSalt` gid
        `Prelude.hashWithSalt` secondaryGids
        `Prelude.hashWithSalt` uid

instance
  Prelude.NFData
    AwsEfsAccessPointPosixUserDetails
  where
  rnf AwsEfsAccessPointPosixUserDetails' {..} =
    Prelude.rnf gid
      `Prelude.seq` Prelude.rnf secondaryGids
      `Prelude.seq` Prelude.rnf uid

instance
  Core.ToJSON
    AwsEfsAccessPointPosixUserDetails
  where
  toJSON AwsEfsAccessPointPosixUserDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Gid" Core..=) Prelude.<$> gid,
            ("SecondaryGids" Core..=) Prelude.<$> secondaryGids,
            ("Uid" Core..=) Prelude.<$> uid
          ]
      )
