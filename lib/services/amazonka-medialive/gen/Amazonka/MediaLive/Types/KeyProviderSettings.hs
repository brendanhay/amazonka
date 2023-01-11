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
-- Module      : Amazonka.MediaLive.Types.KeyProviderSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.KeyProviderSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.StaticKeySettings
import qualified Amazonka.Prelude as Prelude

-- | Key Provider Settings
--
-- /See:/ 'newKeyProviderSettings' smart constructor.
data KeyProviderSettings = KeyProviderSettings'
  { staticKeySettings :: Prelude.Maybe StaticKeySettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyProviderSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticKeySettings', 'keyProviderSettings_staticKeySettings' - Undocumented member.
newKeyProviderSettings ::
  KeyProviderSettings
newKeyProviderSettings =
  KeyProviderSettings'
    { staticKeySettings =
        Prelude.Nothing
    }

-- | Undocumented member.
keyProviderSettings_staticKeySettings :: Lens.Lens' KeyProviderSettings (Prelude.Maybe StaticKeySettings)
keyProviderSettings_staticKeySettings = Lens.lens (\KeyProviderSettings' {staticKeySettings} -> staticKeySettings) (\s@KeyProviderSettings' {} a -> s {staticKeySettings = a} :: KeyProviderSettings)

instance Data.FromJSON KeyProviderSettings where
  parseJSON =
    Data.withObject
      "KeyProviderSettings"
      ( \x ->
          KeyProviderSettings'
            Prelude.<$> (x Data..:? "staticKeySettings")
      )

instance Prelude.Hashable KeyProviderSettings where
  hashWithSalt _salt KeyProviderSettings' {..} =
    _salt `Prelude.hashWithSalt` staticKeySettings

instance Prelude.NFData KeyProviderSettings where
  rnf KeyProviderSettings' {..} =
    Prelude.rnf staticKeySettings

instance Data.ToJSON KeyProviderSettings where
  toJSON KeyProviderSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("staticKeySettings" Data..=)
              Prelude.<$> staticKeySettings
          ]
      )
