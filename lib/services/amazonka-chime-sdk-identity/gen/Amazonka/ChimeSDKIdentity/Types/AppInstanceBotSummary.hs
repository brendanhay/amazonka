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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceBotSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceBotSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High-level information about an AppInstanceBot.
--
-- /See:/ 'newAppInstanceBotSummary' smart constructor.
data AppInstanceBotSummary = AppInstanceBotSummary'
  { -- | The ARN of the AppInstanceBot.
    appInstanceBotArn :: Prelude.Maybe Prelude.Text,
    -- | The metadata of the AppInstanceBot.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the AppInstanceBox.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceBotSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceBotArn', 'appInstanceBotSummary_appInstanceBotArn' - The ARN of the AppInstanceBot.
--
-- 'metadata', 'appInstanceBotSummary_metadata' - The metadata of the AppInstanceBot.
--
-- 'name', 'appInstanceBotSummary_name' - The name of the AppInstanceBox.
newAppInstanceBotSummary ::
  AppInstanceBotSummary
newAppInstanceBotSummary =
  AppInstanceBotSummary'
    { appInstanceBotArn =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the AppInstanceBot.
appInstanceBotSummary_appInstanceBotArn :: Lens.Lens' AppInstanceBotSummary (Prelude.Maybe Prelude.Text)
appInstanceBotSummary_appInstanceBotArn = Lens.lens (\AppInstanceBotSummary' {appInstanceBotArn} -> appInstanceBotArn) (\s@AppInstanceBotSummary' {} a -> s {appInstanceBotArn = a} :: AppInstanceBotSummary)

-- | The metadata of the AppInstanceBot.
appInstanceBotSummary_metadata :: Lens.Lens' AppInstanceBotSummary (Prelude.Maybe Prelude.Text)
appInstanceBotSummary_metadata = Lens.lens (\AppInstanceBotSummary' {metadata} -> metadata) (\s@AppInstanceBotSummary' {} a -> s {metadata = a} :: AppInstanceBotSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the AppInstanceBox.
appInstanceBotSummary_name :: Lens.Lens' AppInstanceBotSummary (Prelude.Maybe Prelude.Text)
appInstanceBotSummary_name = Lens.lens (\AppInstanceBotSummary' {name} -> name) (\s@AppInstanceBotSummary' {} a -> s {name = a} :: AppInstanceBotSummary) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON AppInstanceBotSummary where
  parseJSON =
    Data.withObject
      "AppInstanceBotSummary"
      ( \x ->
          AppInstanceBotSummary'
            Prelude.<$> (x Data..:? "AppInstanceBotArn")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AppInstanceBotSummary where
  hashWithSalt _salt AppInstanceBotSummary' {..} =
    _salt
      `Prelude.hashWithSalt` appInstanceBotArn
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name

instance Prelude.NFData AppInstanceBotSummary where
  rnf AppInstanceBotSummary' {..} =
    Prelude.rnf appInstanceBotArn
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
