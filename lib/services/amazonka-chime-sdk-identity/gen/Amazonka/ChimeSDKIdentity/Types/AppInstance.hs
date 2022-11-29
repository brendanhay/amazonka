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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of an @AppInstance@, an instance of an Amazon Chime SDK
-- messaging application.
--
-- /See:/ 'newAppInstance' smart constructor.
data AppInstance = AppInstance'
  { -- | The time an @AppInstance@ was last updated. In epoch milliseconds.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of an @AppInstance@.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The metadata of an @AppInstance@.
    metadata :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The time at which an @AppInstance@ was created. In epoch milliseconds.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the messaging instance.
    appInstanceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'appInstance_lastUpdatedTimestamp' - The time an @AppInstance@ was last updated. In epoch milliseconds.
--
-- 'name', 'appInstance_name' - The name of an @AppInstance@.
--
-- 'metadata', 'appInstance_metadata' - The metadata of an @AppInstance@.
--
-- 'createdTimestamp', 'appInstance_createdTimestamp' - The time at which an @AppInstance@ was created. In epoch milliseconds.
--
-- 'appInstanceArn', 'appInstance_appInstanceArn' - The ARN of the messaging instance.
newAppInstance ::
  AppInstance
newAppInstance =
  AppInstance'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing,
      metadata = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      appInstanceArn = Prelude.Nothing
    }

-- | The time an @AppInstance@ was last updated. In epoch milliseconds.
appInstance_lastUpdatedTimestamp :: Lens.Lens' AppInstance (Prelude.Maybe Prelude.UTCTime)
appInstance_lastUpdatedTimestamp = Lens.lens (\AppInstance' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@AppInstance' {} a -> s {lastUpdatedTimestamp = a} :: AppInstance) Prelude.. Lens.mapping Core._Time

-- | The name of an @AppInstance@.
appInstance_name :: Lens.Lens' AppInstance (Prelude.Maybe Prelude.Text)
appInstance_name = Lens.lens (\AppInstance' {name} -> name) (\s@AppInstance' {} a -> s {name = a} :: AppInstance) Prelude.. Lens.mapping Core._Sensitive

-- | The metadata of an @AppInstance@.
appInstance_metadata :: Lens.Lens' AppInstance (Prelude.Maybe Prelude.Text)
appInstance_metadata = Lens.lens (\AppInstance' {metadata} -> metadata) (\s@AppInstance' {} a -> s {metadata = a} :: AppInstance) Prelude.. Lens.mapping Core._Sensitive

-- | The time at which an @AppInstance@ was created. In epoch milliseconds.
appInstance_createdTimestamp :: Lens.Lens' AppInstance (Prelude.Maybe Prelude.UTCTime)
appInstance_createdTimestamp = Lens.lens (\AppInstance' {createdTimestamp} -> createdTimestamp) (\s@AppInstance' {} a -> s {createdTimestamp = a} :: AppInstance) Prelude.. Lens.mapping Core._Time

-- | The ARN of the messaging instance.
appInstance_appInstanceArn :: Lens.Lens' AppInstance (Prelude.Maybe Prelude.Text)
appInstance_appInstanceArn = Lens.lens (\AppInstance' {appInstanceArn} -> appInstanceArn) (\s@AppInstance' {} a -> s {appInstanceArn = a} :: AppInstance)

instance Core.FromJSON AppInstance where
  parseJSON =
    Core.withObject
      "AppInstance"
      ( \x ->
          AppInstance'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Metadata")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "AppInstanceArn")
      )

instance Prelude.Hashable AppInstance where
  hashWithSalt _salt AppInstance' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData AppInstance where
  rnf AppInstance' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf appInstanceArn
