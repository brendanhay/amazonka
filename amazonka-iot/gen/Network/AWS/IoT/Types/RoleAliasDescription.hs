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
-- Module      : Network.AWS.IoT.Types.RoleAliasDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RoleAliasDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Role alias description.
--
-- /See:/ 'newRoleAliasDescription' smart constructor.
data RoleAliasDescription = RoleAliasDescription'
  { -- | The ARN of the role alias.
    roleAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the role alias was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The role ARN.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the role alias was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The role alias owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds for which the credential is valid.
    credentialDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The role alias.
    roleAlias :: Prelude.Maybe Prelude.Text
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
-- 'roleAliasArn', 'roleAliasDescription_roleAliasArn' - The ARN of the role alias.
--
-- 'lastModifiedDate', 'roleAliasDescription_lastModifiedDate' - The UNIX timestamp of when the role alias was last modified.
--
-- 'roleArn', 'roleAliasDescription_roleArn' - The role ARN.
--
-- 'creationDate', 'roleAliasDescription_creationDate' - The UNIX timestamp of when the role alias was created.
--
-- 'owner', 'roleAliasDescription_owner' - The role alias owner.
--
-- 'credentialDurationSeconds', 'roleAliasDescription_credentialDurationSeconds' - The number of seconds for which the credential is valid.
--
-- 'roleAlias', 'roleAliasDescription_roleAlias' - The role alias.
newRoleAliasDescription ::
  RoleAliasDescription
newRoleAliasDescription =
  RoleAliasDescription'
    { roleAliasArn =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      owner = Prelude.Nothing,
      credentialDurationSeconds = Prelude.Nothing,
      roleAlias = Prelude.Nothing
    }

-- | The ARN of the role alias.
roleAliasDescription_roleAliasArn :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_roleAliasArn = Lens.lens (\RoleAliasDescription' {roleAliasArn} -> roleAliasArn) (\s@RoleAliasDescription' {} a -> s {roleAliasArn = a} :: RoleAliasDescription)

-- | The UNIX timestamp of when the role alias was last modified.
roleAliasDescription_lastModifiedDate :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.UTCTime)
roleAliasDescription_lastModifiedDate = Lens.lens (\RoleAliasDescription' {lastModifiedDate} -> lastModifiedDate) (\s@RoleAliasDescription' {} a -> s {lastModifiedDate = a} :: RoleAliasDescription) Prelude.. Lens.mapping Core._Time

-- | The role ARN.
roleAliasDescription_roleArn :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_roleArn = Lens.lens (\RoleAliasDescription' {roleArn} -> roleArn) (\s@RoleAliasDescription' {} a -> s {roleArn = a} :: RoleAliasDescription)

-- | The UNIX timestamp of when the role alias was created.
roleAliasDescription_creationDate :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.UTCTime)
roleAliasDescription_creationDate = Lens.lens (\RoleAliasDescription' {creationDate} -> creationDate) (\s@RoleAliasDescription' {} a -> s {creationDate = a} :: RoleAliasDescription) Prelude.. Lens.mapping Core._Time

-- | The role alias owner.
roleAliasDescription_owner :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_owner = Lens.lens (\RoleAliasDescription' {owner} -> owner) (\s@RoleAliasDescription' {} a -> s {owner = a} :: RoleAliasDescription)

-- | The number of seconds for which the credential is valid.
roleAliasDescription_credentialDurationSeconds :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Natural)
roleAliasDescription_credentialDurationSeconds = Lens.lens (\RoleAliasDescription' {credentialDurationSeconds} -> credentialDurationSeconds) (\s@RoleAliasDescription' {} a -> s {credentialDurationSeconds = a} :: RoleAliasDescription)

-- | The role alias.
roleAliasDescription_roleAlias :: Lens.Lens' RoleAliasDescription (Prelude.Maybe Prelude.Text)
roleAliasDescription_roleAlias = Lens.lens (\RoleAliasDescription' {roleAlias} -> roleAlias) (\s@RoleAliasDescription' {} a -> s {roleAlias = a} :: RoleAliasDescription)

instance Core.FromJSON RoleAliasDescription where
  parseJSON =
    Core.withObject
      "RoleAliasDescription"
      ( \x ->
          RoleAliasDescription'
            Prelude.<$> (x Core..:? "roleAliasArn")
            Prelude.<*> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "credentialDurationSeconds")
            Prelude.<*> (x Core..:? "roleAlias")
      )

instance Prelude.Hashable RoleAliasDescription

instance Prelude.NFData RoleAliasDescription
