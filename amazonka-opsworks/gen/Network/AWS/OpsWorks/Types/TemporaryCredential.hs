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
-- Module      : Network.AWS.OpsWorks.Types.TemporaryCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TemporaryCredential where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the data needed by RDP clients such as the Microsoft Remote
-- Desktop Connection to log in to the instance.
--
-- /See:/ 'newTemporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { -- | The length of time (in minutes) that the grant is valid. When the grant
    -- expires, at the end of this period, the user will no longer be able to
    -- use the credentials to log in. If they are logged in at the time, they
    -- will be automatically logged out.
    validForInMinutes :: Core.Maybe Core.Int,
    -- | The instance\'s AWS OpsWorks Stacks ID.
    instanceId :: Core.Maybe Core.Text,
    -- | The password.
    password :: Core.Maybe Core.Text,
    -- | The user name.
    username :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TemporaryCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validForInMinutes', 'temporaryCredential_validForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant
-- expires, at the end of this period, the user will no longer be able to
-- use the credentials to log in. If they are logged in at the time, they
-- will be automatically logged out.
--
-- 'instanceId', 'temporaryCredential_instanceId' - The instance\'s AWS OpsWorks Stacks ID.
--
-- 'password', 'temporaryCredential_password' - The password.
--
-- 'username', 'temporaryCredential_username' - The user name.
newTemporaryCredential ::
  TemporaryCredential
newTemporaryCredential =
  TemporaryCredential'
    { validForInMinutes =
        Core.Nothing,
      instanceId = Core.Nothing,
      password = Core.Nothing,
      username = Core.Nothing
    }

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires, at the end of this period, the user will no longer be able to
-- use the credentials to log in. If they are logged in at the time, they
-- will be automatically logged out.
temporaryCredential_validForInMinutes :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Int)
temporaryCredential_validForInMinutes = Lens.lens (\TemporaryCredential' {validForInMinutes} -> validForInMinutes) (\s@TemporaryCredential' {} a -> s {validForInMinutes = a} :: TemporaryCredential)

-- | The instance\'s AWS OpsWorks Stacks ID.
temporaryCredential_instanceId :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Text)
temporaryCredential_instanceId = Lens.lens (\TemporaryCredential' {instanceId} -> instanceId) (\s@TemporaryCredential' {} a -> s {instanceId = a} :: TemporaryCredential)

-- | The password.
temporaryCredential_password :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Text)
temporaryCredential_password = Lens.lens (\TemporaryCredential' {password} -> password) (\s@TemporaryCredential' {} a -> s {password = a} :: TemporaryCredential)

-- | The user name.
temporaryCredential_username :: Lens.Lens' TemporaryCredential (Core.Maybe Core.Text)
temporaryCredential_username = Lens.lens (\TemporaryCredential' {username} -> username) (\s@TemporaryCredential' {} a -> s {username = a} :: TemporaryCredential)

instance Core.FromJSON TemporaryCredential where
  parseJSON =
    Core.withObject
      "TemporaryCredential"
      ( \x ->
          TemporaryCredential'
            Core.<$> (x Core..:? "ValidForInMinutes")
            Core.<*> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "Username")
      )

instance Core.Hashable TemporaryCredential

instance Core.NFData TemporaryCredential
