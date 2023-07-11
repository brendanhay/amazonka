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
-- Module      : Amazonka.Pinpoint.Types.InAppMessageButton
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessageButton where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.DefaultButtonConfiguration
import Amazonka.Pinpoint.Types.OverrideButtonConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Button Config for an in-app message.
--
-- /See:/ 'newInAppMessageButton' smart constructor.
data InAppMessageButton = InAppMessageButton'
  { -- | Default button content.
    android :: Prelude.Maybe OverrideButtonConfiguration,
    -- | Default button content.
    defaultConfig :: Prelude.Maybe DefaultButtonConfiguration,
    -- | Default button content.
    ios :: Prelude.Maybe OverrideButtonConfiguration,
    -- | Default button content.
    web :: Prelude.Maybe OverrideButtonConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessageButton' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'android', 'inAppMessageButton_android' - Default button content.
--
-- 'defaultConfig', 'inAppMessageButton_defaultConfig' - Default button content.
--
-- 'ios', 'inAppMessageButton_ios' - Default button content.
--
-- 'web', 'inAppMessageButton_web' - Default button content.
newInAppMessageButton ::
  InAppMessageButton
newInAppMessageButton =
  InAppMessageButton'
    { android = Prelude.Nothing,
      defaultConfig = Prelude.Nothing,
      ios = Prelude.Nothing,
      web = Prelude.Nothing
    }

-- | Default button content.
inAppMessageButton_android :: Lens.Lens' InAppMessageButton (Prelude.Maybe OverrideButtonConfiguration)
inAppMessageButton_android = Lens.lens (\InAppMessageButton' {android} -> android) (\s@InAppMessageButton' {} a -> s {android = a} :: InAppMessageButton)

-- | Default button content.
inAppMessageButton_defaultConfig :: Lens.Lens' InAppMessageButton (Prelude.Maybe DefaultButtonConfiguration)
inAppMessageButton_defaultConfig = Lens.lens (\InAppMessageButton' {defaultConfig} -> defaultConfig) (\s@InAppMessageButton' {} a -> s {defaultConfig = a} :: InAppMessageButton)

-- | Default button content.
inAppMessageButton_ios :: Lens.Lens' InAppMessageButton (Prelude.Maybe OverrideButtonConfiguration)
inAppMessageButton_ios = Lens.lens (\InAppMessageButton' {ios} -> ios) (\s@InAppMessageButton' {} a -> s {ios = a} :: InAppMessageButton)

-- | Default button content.
inAppMessageButton_web :: Lens.Lens' InAppMessageButton (Prelude.Maybe OverrideButtonConfiguration)
inAppMessageButton_web = Lens.lens (\InAppMessageButton' {web} -> web) (\s@InAppMessageButton' {} a -> s {web = a} :: InAppMessageButton)

instance Data.FromJSON InAppMessageButton where
  parseJSON =
    Data.withObject
      "InAppMessageButton"
      ( \x ->
          InAppMessageButton'
            Prelude.<$> (x Data..:? "Android")
            Prelude.<*> (x Data..:? "DefaultConfig")
            Prelude.<*> (x Data..:? "IOS")
            Prelude.<*> (x Data..:? "Web")
      )

instance Prelude.Hashable InAppMessageButton where
  hashWithSalt _salt InAppMessageButton' {..} =
    _salt
      `Prelude.hashWithSalt` android
      `Prelude.hashWithSalt` defaultConfig
      `Prelude.hashWithSalt` ios
      `Prelude.hashWithSalt` web

instance Prelude.NFData InAppMessageButton where
  rnf InAppMessageButton' {..} =
    Prelude.rnf android
      `Prelude.seq` Prelude.rnf defaultConfig
      `Prelude.seq` Prelude.rnf ios
      `Prelude.seq` Prelude.rnf web

instance Data.ToJSON InAppMessageButton where
  toJSON InAppMessageButton' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Android" Data..=) Prelude.<$> android,
            ("DefaultConfig" Data..=) Prelude.<$> defaultConfig,
            ("IOS" Data..=) Prelude.<$> ios,
            ("Web" Data..=) Prelude.<$> web
          ]
      )
