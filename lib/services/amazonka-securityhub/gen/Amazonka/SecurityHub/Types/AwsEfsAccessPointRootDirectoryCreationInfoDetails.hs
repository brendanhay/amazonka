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
-- Module      : Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryCreationInfoDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryCreationInfoDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the settings that Amazon EFS uses to create
-- the root directory when a client connects to an access point.
--
-- /See:/ 'newAwsEfsAccessPointRootDirectoryCreationInfoDetails' smart constructor.
data AwsEfsAccessPointRootDirectoryCreationInfoDetails = AwsEfsAccessPointRootDirectoryCreationInfoDetails'
  { -- | Specifies the POSIX permissions to apply to the root directory, in the
    -- format of an octal number representing the file\'s mode bits.
    permissions :: Prelude.Maybe Prelude.Text,
    -- | Specifies the POSIX group ID to apply to the root directory.
    ownerGid :: Prelude.Maybe Prelude.Text,
    -- | Specifies the POSIX user ID to apply to the root directory.
    ownerUid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEfsAccessPointRootDirectoryCreationInfoDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions' - Specifies the POSIX permissions to apply to the root directory, in the
-- format of an octal number representing the file\'s mode bits.
--
-- 'ownerGid', 'awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid' - Specifies the POSIX group ID to apply to the root directory.
--
-- 'ownerUid', 'awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid' - Specifies the POSIX user ID to apply to the root directory.
newAwsEfsAccessPointRootDirectoryCreationInfoDetails ::
  AwsEfsAccessPointRootDirectoryCreationInfoDetails
newAwsEfsAccessPointRootDirectoryCreationInfoDetails =
  AwsEfsAccessPointRootDirectoryCreationInfoDetails'
    { permissions =
        Prelude.Nothing,
      ownerGid =
        Prelude.Nothing,
      ownerUid =
        Prelude.Nothing
    }

-- | Specifies the POSIX permissions to apply to the root directory, in the
-- format of an octal number representing the file\'s mode bits.
awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions :: Lens.Lens' AwsEfsAccessPointRootDirectoryCreationInfoDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointRootDirectoryCreationInfoDetails_permissions = Lens.lens (\AwsEfsAccessPointRootDirectoryCreationInfoDetails' {permissions} -> permissions) (\s@AwsEfsAccessPointRootDirectoryCreationInfoDetails' {} a -> s {permissions = a} :: AwsEfsAccessPointRootDirectoryCreationInfoDetails)

-- | Specifies the POSIX group ID to apply to the root directory.
awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid :: Lens.Lens' AwsEfsAccessPointRootDirectoryCreationInfoDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerGid = Lens.lens (\AwsEfsAccessPointRootDirectoryCreationInfoDetails' {ownerGid} -> ownerGid) (\s@AwsEfsAccessPointRootDirectoryCreationInfoDetails' {} a -> s {ownerGid = a} :: AwsEfsAccessPointRootDirectoryCreationInfoDetails)

-- | Specifies the POSIX user ID to apply to the root directory.
awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid :: Lens.Lens' AwsEfsAccessPointRootDirectoryCreationInfoDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointRootDirectoryCreationInfoDetails_ownerUid = Lens.lens (\AwsEfsAccessPointRootDirectoryCreationInfoDetails' {ownerUid} -> ownerUid) (\s@AwsEfsAccessPointRootDirectoryCreationInfoDetails' {} a -> s {ownerUid = a} :: AwsEfsAccessPointRootDirectoryCreationInfoDetails)

instance
  Core.FromJSON
    AwsEfsAccessPointRootDirectoryCreationInfoDetails
  where
  parseJSON =
    Core.withObject
      "AwsEfsAccessPointRootDirectoryCreationInfoDetails"
      ( \x ->
          AwsEfsAccessPointRootDirectoryCreationInfoDetails'
            Prelude.<$> (x Core..:? "Permissions")
              Prelude.<*> (x Core..:? "OwnerGid")
              Prelude.<*> (x Core..:? "OwnerUid")
      )

instance
  Prelude.Hashable
    AwsEfsAccessPointRootDirectoryCreationInfoDetails
  where
  hashWithSalt
    _salt
    AwsEfsAccessPointRootDirectoryCreationInfoDetails' {..} =
      _salt `Prelude.hashWithSalt` permissions
        `Prelude.hashWithSalt` ownerGid
        `Prelude.hashWithSalt` ownerUid

instance
  Prelude.NFData
    AwsEfsAccessPointRootDirectoryCreationInfoDetails
  where
  rnf
    AwsEfsAccessPointRootDirectoryCreationInfoDetails' {..} =
      Prelude.rnf permissions
        `Prelude.seq` Prelude.rnf ownerGid
        `Prelude.seq` Prelude.rnf ownerUid

instance
  Core.ToJSON
    AwsEfsAccessPointRootDirectoryCreationInfoDetails
  where
  toJSON
    AwsEfsAccessPointRootDirectoryCreationInfoDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Permissions" Core..=) Prelude.<$> permissions,
              ("OwnerGid" Core..=) Prelude.<$> ownerGid,
              ("OwnerUid" Core..=) Prelude.<$> ownerUid
            ]
        )
