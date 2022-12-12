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
-- Module      : Amazonka.IoT.Types.RoleAliasDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.RoleAliasDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Role alias description.
--
-- /See:/ 'newRoleAliasDescription' smart constructor.
data RoleAliasDescription = RoleAliasDescription'
  { -- | The UNIX timestamp of when the role alias was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The number of seconds for which the credential is valid.
    credentialDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The UNIX timestamp of when the role alias was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The role alias owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The role alias.
    roleAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role alias.
    roleAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The role ARN.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoleAliasDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'roleAliasDescription_creationDate' - The UNIX timestamp of when the role alias was created.
--
-- 'credentialDurationSeconds', 'roleAliasDescription_credentialDurationSeconds' - The number of seconds for which the credential is valid.
--
-- 'lastModifiedDate', 'roleAliasDescription_lastModifiedDate' - The UNIX timestamp of when the role alias was last modified.
--
-- 'owner', 'roleAliasDescription_owner' - The role alias owner.
--
-- 'roleAlias', 'roleAliasDescription_roleAlias' - The role alias.
--
-- 'roleAliasArn', 'roleAliasDescription_roleAliasArn' - The ARN of the role alias.
--
-- 'roleArn', 'roleAliasDescription_roleArn' - The role ARN.
newRoleAliasDescription ::
  RoleAliasDescription
newRoleAliasDescription =
  RoleAliasDescription'
    { creationDate =
        Prelude.Nothing,
      credentialDurationSeconds = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      owner = Prelude.Nothing,
      roleAlias = Prelude.Nothing,
      roleAliasArn = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The UNIX timestamp of when the role alias was created.
roleAliasDescription_creationDate :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.UTCTime)
roleAliasDescription_creationDate = Lens.lens (\RoleAliasDescription' {creationDate} -> creationDate) (\s@RoleAliasDescription' {} a -> s {creationDate = a} :: RoleAliasDescription) Prelude.. Lens.mapping Data._Time

-- | The number of seconds for which the credential is valid.
roleAliasDescription_credentialDurationSeconds :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Natural)
roleAliasDescription_credentialDurationSeconds = Lens.lens (\RoleAliasDescription' {credentialDurationSeconds} -> credentialDurationSeconds) (\s@RoleAliasDescription' {} a -> s {credentialDurationSeconds = a} :: RoleAliasDescription)

-- | The UNIX timestamp of when the role alias was last modified.
roleAliasDescription_lastModifiedDate :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.UTCTime)
roleAliasDescription_lastModifiedDate = Lens.lens (\RoleAliasDescription' {lastModifiedDate} -> lastModifiedDate) (\s@RoleAliasDescription' {} a -> s {lastModifiedDate = a} :: RoleAliasDescription) Prelude.. Lens.mapping Data._Time

-- | The role alias owner.
roleAliasDescription_owner :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_owner = Lens.lens (\RoleAliasDescription' {owner} -> owner) (\s@RoleAliasDescription' {} a -> s {owner = a} :: RoleAliasDescription)

-- | The role alias.
roleAliasDescription_roleAlias :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_roleAlias = Lens.lens (\RoleAliasDescription' {roleAlias} -> roleAlias) (\s@RoleAliasDescription' {} a -> s {roleAlias = a} :: RoleAliasDescription)

-- | The ARN of the role alias.
roleAliasDescription_roleAliasArn :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_roleAliasArn = Lens.lens (\RoleAliasDescription' {roleAliasArn} -> roleAliasArn) (\s@RoleAliasDescription' {} a -> s {roleAliasArn = a} :: RoleAliasDescription)

-- | The role ARN.
roleAliasDescription_roleArn :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_roleArn = Lens.lens (\RoleAliasDescription' {roleArn} -> roleArn) (\s@RoleAliasDescription' {} a -> s {roleArn = a} :: RoleAliasDescription)

instance Data.FromJSON RoleAliasDescription where
  parseJSON =
    Data.withObject
      "RoleAliasDescription"
      ( \x ->
          RoleAliasDescription'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "credentialDurationSeconds")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "roleAlias")
            Prelude.<*> (x Data..:? "roleAliasArn")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance Prelude.Hashable RoleAliasDescription where
  hashWithSalt _salt RoleAliasDescription' {..} =
    _salt `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` credentialDurationSeconds
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` roleAlias
      `Prelude.hashWithSalt` roleAliasArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData RoleAliasDescription where
  rnf RoleAliasDescription' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf credentialDurationSeconds
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf roleAlias
      `Prelude.seq` Prelude.rnf roleAliasArn
      `Prelude.seq` Prelude.rnf roleArn
