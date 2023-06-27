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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceBot where

import Amazonka.ChimeSDKIdentity.Types.Configuration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Lex V2 chat bot created under an @AppInstance@.
--
-- /See:/ 'newAppInstanceBot' smart constructor.
data AppInstanceBot = AppInstanceBot'
  { -- | The ARN of the AppInstanceBot.
    appInstanceBotArn :: Prelude.Maybe Prelude.Text,
    -- | The data processing instructions for an AppInstanceBot.
    configuration :: Prelude.Maybe Configuration,
    -- | The time at which the @AppInstanceBot@ was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The time at which the @AppInstanceBot@ was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The metadata for an AppInstanceBot.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the AppInstanceBot.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBotArn', 'appInstanceBot_appInstanceBotArn' - The ARN of the AppInstanceBot.
--
-- 'configuration', 'appInstanceBot_configuration' - The data processing instructions for an AppInstanceBot.
--
-- 'createdTimestamp', 'appInstanceBot_createdTimestamp' - The time at which the @AppInstanceBot@ was created.
--
-- 'lastUpdatedTimestamp', 'appInstanceBot_lastUpdatedTimestamp' - The time at which the @AppInstanceBot@ was last updated.
--
-- 'metadata', 'appInstanceBot_metadata' - The metadata for an AppInstanceBot.
--
-- 'name', 'appInstanceBot_name' - The name of the AppInstanceBot.
newAppInstanceBot ::
  AppInstanceBot
newAppInstanceBot =
  AppInstanceBot'
    { appInstanceBotArn =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the AppInstanceBot.
appInstanceBot_appInstanceBotArn :: Lens.Lens' AppInstanceBot (Prelude.Maybe Prelude.Text)
appInstanceBot_appInstanceBotArn = Lens.lens (\AppInstanceBot' {appInstanceBotArn} -> appInstanceBotArn) (\s@AppInstanceBot' {} a -> s {appInstanceBotArn = a} :: AppInstanceBot)

-- | The data processing instructions for an AppInstanceBot.
appInstanceBot_configuration :: Lens.Lens' AppInstanceBot (Prelude.Maybe Configuration)
appInstanceBot_configuration = Lens.lens (\AppInstanceBot' {configuration} -> configuration) (\s@AppInstanceBot' {} a -> s {configuration = a} :: AppInstanceBot)

-- | The time at which the @AppInstanceBot@ was created.
appInstanceBot_createdTimestamp :: Lens.Lens' AppInstanceBot (Prelude.Maybe Prelude.UTCTime)
appInstanceBot_createdTimestamp = Lens.lens (\AppInstanceBot' {createdTimestamp} -> createdTimestamp) (\s@AppInstanceBot' {} a -> s {createdTimestamp = a} :: AppInstanceBot) Prelude.. Lens.mapping Data._Time

-- | The time at which the @AppInstanceBot@ was last updated.
appInstanceBot_lastUpdatedTimestamp :: Lens.Lens' AppInstanceBot (Prelude.Maybe Prelude.UTCTime)
appInstanceBot_lastUpdatedTimestamp = Lens.lens (\AppInstanceBot' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@AppInstanceBot' {} a -> s {lastUpdatedTimestamp = a} :: AppInstanceBot) Prelude.. Lens.mapping Data._Time

-- | The metadata for an AppInstanceBot.
appInstanceBot_metadata :: Lens.Lens' AppInstanceBot (Prelude.Maybe Prelude.Text)
appInstanceBot_metadata = Lens.lens (\AppInstanceBot' {metadata} -> metadata) (\s@AppInstanceBot' {} a -> s {metadata = a} :: AppInstanceBot) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the AppInstanceBot.
appInstanceBot_name :: Lens.Lens' AppInstanceBot (Prelude.Maybe Prelude.Text)
appInstanceBot_name = Lens.lens (\AppInstanceBot' {name} -> name) (\s@AppInstanceBot' {} a -> s {name = a} :: AppInstanceBot) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON AppInstanceBot where
  parseJSON =
    Data.withObject
      "AppInstanceBot"
      ( \x ->
          AppInstanceBot'
            Prelude.<$> (x Data..:? "AppInstanceBotArn")
            Prelude.<*> (x Data..:? "Configuration")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AppInstanceBot where
  hashWithSalt _salt AppInstanceBot' {..} =
    _salt
      `Prelude.hashWithSalt` appInstanceBotArn
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name

instance Prelude.NFData AppInstanceBot where
  rnf AppInstanceBot' {..} =
    Prelude.rnf appInstanceBotArn
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
