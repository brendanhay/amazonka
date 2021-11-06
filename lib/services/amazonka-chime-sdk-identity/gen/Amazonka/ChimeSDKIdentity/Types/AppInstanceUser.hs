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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of an @AppInstanceUser@.
--
-- /See:/ 'newAppInstanceUser' smart constructor.
data AppInstanceUser = AppInstanceUser'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the @AppInstanceUser@.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The metadata of the @AppInstanceUser@.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The time at which the @AppInstanceUser@ was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The time at which the @AppInstanceUser@ was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'appInstanceUser_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'name', 'appInstanceUser_name' - The name of the @AppInstanceUser@.
--
-- 'metadata', 'appInstanceUser_metadata' - The metadata of the @AppInstanceUser@.
--
-- 'createdTimestamp', 'appInstanceUser_createdTimestamp' - The time at which the @AppInstanceUser@ was created.
--
-- 'lastUpdatedTimestamp', 'appInstanceUser_lastUpdatedTimestamp' - The time at which the @AppInstanceUser@ was last updated.
newAppInstanceUser ::
  AppInstanceUser
newAppInstanceUser =
  AppInstanceUser'
    { appInstanceUserArn =
        Prelude.Nothing,
      name = Prelude.Nothing,
      metadata = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing
    }

-- | The ARN of the @AppInstanceUser@.
appInstanceUser_appInstanceUserArn :: Lens.Lens' AppInstanceUser (Prelude.Maybe Prelude.Text)
appInstanceUser_appInstanceUserArn = Lens.lens (\AppInstanceUser' {appInstanceUserArn} -> appInstanceUserArn) (\s@AppInstanceUser' {} a -> s {appInstanceUserArn = a} :: AppInstanceUser)

-- | The name of the @AppInstanceUser@.
appInstanceUser_name :: Lens.Lens' AppInstanceUser (Prelude.Maybe Prelude.Text)
appInstanceUser_name = Lens.lens (\AppInstanceUser' {name} -> name) (\s@AppInstanceUser' {} a -> s {name = a} :: AppInstanceUser) Prelude.. Lens.mapping Core._Sensitive

-- | The metadata of the @AppInstanceUser@.
appInstanceUser_metadata :: Lens.Lens' AppInstanceUser (Prelude.Maybe Prelude.Text)
appInstanceUser_metadata = Lens.lens (\AppInstanceUser' {metadata} -> metadata) (\s@AppInstanceUser' {} a -> s {metadata = a} :: AppInstanceUser) Prelude.. Lens.mapping Core._Sensitive

-- | The time at which the @AppInstanceUser@ was created.
appInstanceUser_createdTimestamp :: Lens.Lens' AppInstanceUser (Prelude.Maybe Prelude.UTCTime)
appInstanceUser_createdTimestamp = Lens.lens (\AppInstanceUser' {createdTimestamp} -> createdTimestamp) (\s@AppInstanceUser' {} a -> s {createdTimestamp = a} :: AppInstanceUser) Prelude.. Lens.mapping Core._Time

-- | The time at which the @AppInstanceUser@ was last updated.
appInstanceUser_lastUpdatedTimestamp :: Lens.Lens' AppInstanceUser (Prelude.Maybe Prelude.UTCTime)
appInstanceUser_lastUpdatedTimestamp = Lens.lens (\AppInstanceUser' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@AppInstanceUser' {} a -> s {lastUpdatedTimestamp = a} :: AppInstanceUser) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AppInstanceUser where
  parseJSON =
    Core.withObject
      "AppInstanceUser"
      ( \x ->
          AppInstanceUser'
            Prelude.<$> (x Core..:? "AppInstanceUserArn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "LastUpdatedTimestamp")
      )

instance Prelude.Hashable AppInstanceUser

instance Prelude.NFData AppInstanceUser
