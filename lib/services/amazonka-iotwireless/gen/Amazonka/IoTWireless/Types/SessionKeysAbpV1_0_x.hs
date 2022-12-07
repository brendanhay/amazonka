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
-- Module      : Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Session keys for ABP v1.1
--
-- /See:/ 'newSessionKeysAbpV1_0_x' smart constructor.
data SessionKeysAbpV1_0_x = SessionKeysAbpV1_0_x'
  { -- | The NwkSKey value.
    nwkSKey :: Prelude.Maybe Prelude.Text,
    -- | The AppSKey value.
    appSKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeysAbpV1_0_x' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nwkSKey', 'sessionKeysAbpV1_0_x_nwkSKey' - The NwkSKey value.
--
-- 'appSKey', 'sessionKeysAbpV1_0_x_appSKey' - The AppSKey value.
newSessionKeysAbpV1_0_x ::
  SessionKeysAbpV1_0_x
newSessionKeysAbpV1_0_x =
  SessionKeysAbpV1_0_x'
    { nwkSKey = Prelude.Nothing,
      appSKey = Prelude.Nothing
    }

-- | The NwkSKey value.
sessionKeysAbpV1_0_x_nwkSKey :: Lens.Lens' SessionKeysAbpV1_0_x (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_0_x_nwkSKey = Lens.lens (\SessionKeysAbpV1_0_x' {nwkSKey} -> nwkSKey) (\s@SessionKeysAbpV1_0_x' {} a -> s {nwkSKey = a} :: SessionKeysAbpV1_0_x)

-- | The AppSKey value.
sessionKeysAbpV1_0_x_appSKey :: Lens.Lens' SessionKeysAbpV1_0_x (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_0_x_appSKey = Lens.lens (\SessionKeysAbpV1_0_x' {appSKey} -> appSKey) (\s@SessionKeysAbpV1_0_x' {} a -> s {appSKey = a} :: SessionKeysAbpV1_0_x)

instance Data.FromJSON SessionKeysAbpV1_0_x where
  parseJSON =
    Data.withObject
      "SessionKeysAbpV1_0_x"
      ( \x ->
          SessionKeysAbpV1_0_x'
            Prelude.<$> (x Data..:? "NwkSKey")
            Prelude.<*> (x Data..:? "AppSKey")
      )

instance Prelude.Hashable SessionKeysAbpV1_0_x where
  hashWithSalt _salt SessionKeysAbpV1_0_x' {..} =
    _salt `Prelude.hashWithSalt` nwkSKey
      `Prelude.hashWithSalt` appSKey

instance Prelude.NFData SessionKeysAbpV1_0_x where
  rnf SessionKeysAbpV1_0_x' {..} =
    Prelude.rnf nwkSKey
      `Prelude.seq` Prelude.rnf appSKey

instance Data.ToJSON SessionKeysAbpV1_0_x where
  toJSON SessionKeysAbpV1_0_x' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NwkSKey" Data..=) Prelude.<$> nwkSKey,
            ("AppSKey" Data..=) Prelude.<$> appSKey
          ]
      )
