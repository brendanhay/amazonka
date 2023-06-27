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
-- Module      : Amazonka.FinSpace.Types.KxUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that stores metadata for a kdb user.
--
-- /See:/ 'newKxUser' smart constructor.
data KxUser = KxUser'
  { -- | The timestamp at which the kdb user was created.
    createTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The IAM role ARN that is associated with the user.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the kdb user was updated.
    updateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) that identifies the user. For more
    -- information about ARNs and how to use ARNs in policies, see
    -- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
    -- /IAM User Guide/.
    userArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the user.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTimestamp', 'kxUser_createTimestamp' - The timestamp at which the kdb user was created.
--
-- 'iamRole', 'kxUser_iamRole' - The IAM role ARN that is associated with the user.
--
-- 'updateTimestamp', 'kxUser_updateTimestamp' - The timestamp at which the kdb user was updated.
--
-- 'userArn', 'kxUser_userArn' - The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
--
-- 'userName', 'kxUser_userName' - A unique identifier for the user.
newKxUser ::
  KxUser
newKxUser =
  KxUser'
    { createTimestamp = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      updateTimestamp = Prelude.Nothing,
      userArn = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The timestamp at which the kdb user was created.
kxUser_createTimestamp :: Lens.Lens' KxUser (Prelude.Maybe Prelude.UTCTime)
kxUser_createTimestamp = Lens.lens (\KxUser' {createTimestamp} -> createTimestamp) (\s@KxUser' {} a -> s {createTimestamp = a} :: KxUser) Prelude.. Lens.mapping Data._Time

-- | The IAM role ARN that is associated with the user.
kxUser_iamRole :: Lens.Lens' KxUser (Prelude.Maybe Prelude.Text)
kxUser_iamRole = Lens.lens (\KxUser' {iamRole} -> iamRole) (\s@KxUser' {} a -> s {iamRole = a} :: KxUser)

-- | The timestamp at which the kdb user was updated.
kxUser_updateTimestamp :: Lens.Lens' KxUser (Prelude.Maybe Prelude.UTCTime)
kxUser_updateTimestamp = Lens.lens (\KxUser' {updateTimestamp} -> updateTimestamp) (\s@KxUser' {} a -> s {updateTimestamp = a} :: KxUser) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
kxUser_userArn :: Lens.Lens' KxUser (Prelude.Maybe Prelude.Text)
kxUser_userArn = Lens.lens (\KxUser' {userArn} -> userArn) (\s@KxUser' {} a -> s {userArn = a} :: KxUser)

-- | A unique identifier for the user.
kxUser_userName :: Lens.Lens' KxUser (Prelude.Maybe Prelude.Text)
kxUser_userName = Lens.lens (\KxUser' {userName} -> userName) (\s@KxUser' {} a -> s {userName = a} :: KxUser)

instance Data.FromJSON KxUser where
  parseJSON =
    Data.withObject
      "KxUser"
      ( \x ->
          KxUser'
            Prelude.<$> (x Data..:? "createTimestamp")
            Prelude.<*> (x Data..:? "iamRole")
            Prelude.<*> (x Data..:? "updateTimestamp")
            Prelude.<*> (x Data..:? "userArn")
            Prelude.<*> (x Data..:? "userName")
      )

instance Prelude.Hashable KxUser where
  hashWithSalt _salt KxUser' {..} =
    _salt
      `Prelude.hashWithSalt` createTimestamp
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` updateTimestamp
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` userName

instance Prelude.NFData KxUser where
  rnf KxUser' {..} =
    Prelude.rnf createTimestamp
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf updateTimestamp
      `Prelude.seq` Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf userName
