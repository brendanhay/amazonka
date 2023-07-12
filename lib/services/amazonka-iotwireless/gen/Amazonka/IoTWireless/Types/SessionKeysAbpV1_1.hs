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
-- Module      : Amazonka.IoTWireless.Types.SessionKeysAbpV1_1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SessionKeysAbpV1_1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Session keys for ABP v1.1
--
-- /See:/ 'newSessionKeysAbpV1_1' smart constructor.
data SessionKeysAbpV1_1 = SessionKeysAbpV1_1'
  { -- | The AppSKey value.
    appSKey :: Prelude.Maybe Prelude.Text,
    -- | The FNwkSIntKey value.
    fNwkSIntKey :: Prelude.Maybe Prelude.Text,
    -- | The NwkSEncKey value.
    nwkSEncKey :: Prelude.Maybe Prelude.Text,
    -- | The SNwkSIntKey value.
    sNwkSIntKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeysAbpV1_1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appSKey', 'sessionKeysAbpV1_1_appSKey' - The AppSKey value.
--
-- 'fNwkSIntKey', 'sessionKeysAbpV1_1_fNwkSIntKey' - The FNwkSIntKey value.
--
-- 'nwkSEncKey', 'sessionKeysAbpV1_1_nwkSEncKey' - The NwkSEncKey value.
--
-- 'sNwkSIntKey', 'sessionKeysAbpV1_1_sNwkSIntKey' - The SNwkSIntKey value.
newSessionKeysAbpV1_1 ::
  SessionKeysAbpV1_1
newSessionKeysAbpV1_1 =
  SessionKeysAbpV1_1'
    { appSKey = Prelude.Nothing,
      fNwkSIntKey = Prelude.Nothing,
      nwkSEncKey = Prelude.Nothing,
      sNwkSIntKey = Prelude.Nothing
    }

-- | The AppSKey value.
sessionKeysAbpV1_1_appSKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_appSKey = Lens.lens (\SessionKeysAbpV1_1' {appSKey} -> appSKey) (\s@SessionKeysAbpV1_1' {} a -> s {appSKey = a} :: SessionKeysAbpV1_1)

-- | The FNwkSIntKey value.
sessionKeysAbpV1_1_fNwkSIntKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_fNwkSIntKey = Lens.lens (\SessionKeysAbpV1_1' {fNwkSIntKey} -> fNwkSIntKey) (\s@SessionKeysAbpV1_1' {} a -> s {fNwkSIntKey = a} :: SessionKeysAbpV1_1)

-- | The NwkSEncKey value.
sessionKeysAbpV1_1_nwkSEncKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_nwkSEncKey = Lens.lens (\SessionKeysAbpV1_1' {nwkSEncKey} -> nwkSEncKey) (\s@SessionKeysAbpV1_1' {} a -> s {nwkSEncKey = a} :: SessionKeysAbpV1_1)

-- | The SNwkSIntKey value.
sessionKeysAbpV1_1_sNwkSIntKey :: Lens.Lens' SessionKeysAbpV1_1 (Prelude.Maybe Prelude.Text)
sessionKeysAbpV1_1_sNwkSIntKey = Lens.lens (\SessionKeysAbpV1_1' {sNwkSIntKey} -> sNwkSIntKey) (\s@SessionKeysAbpV1_1' {} a -> s {sNwkSIntKey = a} :: SessionKeysAbpV1_1)

instance Data.FromJSON SessionKeysAbpV1_1 where
  parseJSON =
    Data.withObject
      "SessionKeysAbpV1_1"
      ( \x ->
          SessionKeysAbpV1_1'
            Prelude.<$> (x Data..:? "AppSKey")
            Prelude.<*> (x Data..:? "FNwkSIntKey")
            Prelude.<*> (x Data..:? "NwkSEncKey")
            Prelude.<*> (x Data..:? "SNwkSIntKey")
      )

instance Prelude.Hashable SessionKeysAbpV1_1 where
  hashWithSalt _salt SessionKeysAbpV1_1' {..} =
    _salt
      `Prelude.hashWithSalt` appSKey
      `Prelude.hashWithSalt` fNwkSIntKey
      `Prelude.hashWithSalt` nwkSEncKey
      `Prelude.hashWithSalt` sNwkSIntKey

instance Prelude.NFData SessionKeysAbpV1_1 where
  rnf SessionKeysAbpV1_1' {..} =
    Prelude.rnf appSKey
      `Prelude.seq` Prelude.rnf fNwkSIntKey
      `Prelude.seq` Prelude.rnf nwkSEncKey
      `Prelude.seq` Prelude.rnf sNwkSIntKey

instance Data.ToJSON SessionKeysAbpV1_1 where
  toJSON SessionKeysAbpV1_1' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppSKey" Data..=) Prelude.<$> appSKey,
            ("FNwkSIntKey" Data..=) Prelude.<$> fNwkSIntKey,
            ("NwkSEncKey" Data..=) Prelude.<$> nwkSEncKey,
            ("SNwkSIntKey" Data..=) Prelude.<$> sNwkSIntKey
          ]
      )
