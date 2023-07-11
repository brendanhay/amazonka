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
-- Module      : Amazonka.OpsWorks.Types.SelfUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.SelfUserProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a user\'s SSH information.
--
-- /See:/ 'newSelfUserProfile' smart constructor.
data SelfUserProfile = SelfUserProfile'
  { -- | The user\'s IAM ARN.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The user\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH public key.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH user name.
    sshUsername :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserArn', 'selfUserProfile_iamUserArn' - The user\'s IAM ARN.
--
-- 'name', 'selfUserProfile_name' - The user\'s name.
--
-- 'sshPublicKey', 'selfUserProfile_sshPublicKey' - The user\'s SSH public key.
--
-- 'sshUsername', 'selfUserProfile_sshUsername' - The user\'s SSH user name.
newSelfUserProfile ::
  SelfUserProfile
newSelfUserProfile =
  SelfUserProfile'
    { iamUserArn = Prelude.Nothing,
      name = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      sshUsername = Prelude.Nothing
    }

-- | The user\'s IAM ARN.
selfUserProfile_iamUserArn :: Lens.Lens' SelfUserProfile (Prelude.Maybe Prelude.Text)
selfUserProfile_iamUserArn = Lens.lens (\SelfUserProfile' {iamUserArn} -> iamUserArn) (\s@SelfUserProfile' {} a -> s {iamUserArn = a} :: SelfUserProfile)

-- | The user\'s name.
selfUserProfile_name :: Lens.Lens' SelfUserProfile (Prelude.Maybe Prelude.Text)
selfUserProfile_name = Lens.lens (\SelfUserProfile' {name} -> name) (\s@SelfUserProfile' {} a -> s {name = a} :: SelfUserProfile)

-- | The user\'s SSH public key.
selfUserProfile_sshPublicKey :: Lens.Lens' SelfUserProfile (Prelude.Maybe Prelude.Text)
selfUserProfile_sshPublicKey = Lens.lens (\SelfUserProfile' {sshPublicKey} -> sshPublicKey) (\s@SelfUserProfile' {} a -> s {sshPublicKey = a} :: SelfUserProfile)

-- | The user\'s SSH user name.
selfUserProfile_sshUsername :: Lens.Lens' SelfUserProfile (Prelude.Maybe Prelude.Text)
selfUserProfile_sshUsername = Lens.lens (\SelfUserProfile' {sshUsername} -> sshUsername) (\s@SelfUserProfile' {} a -> s {sshUsername = a} :: SelfUserProfile)

instance Data.FromJSON SelfUserProfile where
  parseJSON =
    Data.withObject
      "SelfUserProfile"
      ( \x ->
          SelfUserProfile'
            Prelude.<$> (x Data..:? "IamUserArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SshPublicKey")
            Prelude.<*> (x Data..:? "SshUsername")
      )

instance Prelude.Hashable SelfUserProfile where
  hashWithSalt _salt SelfUserProfile' {..} =
    _salt
      `Prelude.hashWithSalt` iamUserArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sshPublicKey
      `Prelude.hashWithSalt` sshUsername

instance Prelude.NFData SelfUserProfile where
  rnf SelfUserProfile' {..} =
    Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf sshUsername
