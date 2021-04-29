{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.StaticKeySettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticKeySettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Prelude

-- | Static Key Settings
--
-- /See:/ 'newStaticKeySettings' smart constructor.
data StaticKeySettings = StaticKeySettings'
  { -- | The URL of the license server used for protecting content.
    keyProviderServer :: Prelude.Maybe InputLocation,
    -- | Static key value as a 32 character hexadecimal string.
    staticKeyValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StaticKeySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyProviderServer', 'staticKeySettings_keyProviderServer' - The URL of the license server used for protecting content.
--
-- 'staticKeyValue', 'staticKeySettings_staticKeyValue' - Static key value as a 32 character hexadecimal string.
newStaticKeySettings ::
  -- | 'staticKeyValue'
  Prelude.Text ->
  StaticKeySettings
newStaticKeySettings pStaticKeyValue_ =
  StaticKeySettings'
    { keyProviderServer =
        Prelude.Nothing,
      staticKeyValue = pStaticKeyValue_
    }

-- | The URL of the license server used for protecting content.
staticKeySettings_keyProviderServer :: Lens.Lens' StaticKeySettings (Prelude.Maybe InputLocation)
staticKeySettings_keyProviderServer = Lens.lens (\StaticKeySettings' {keyProviderServer} -> keyProviderServer) (\s@StaticKeySettings' {} a -> s {keyProviderServer = a} :: StaticKeySettings)

-- | Static key value as a 32 character hexadecimal string.
staticKeySettings_staticKeyValue :: Lens.Lens' StaticKeySettings Prelude.Text
staticKeySettings_staticKeyValue = Lens.lens (\StaticKeySettings' {staticKeyValue} -> staticKeyValue) (\s@StaticKeySettings' {} a -> s {staticKeyValue = a} :: StaticKeySettings)

instance Prelude.FromJSON StaticKeySettings where
  parseJSON =
    Prelude.withObject
      "StaticKeySettings"
      ( \x ->
          StaticKeySettings'
            Prelude.<$> (x Prelude..:? "keyProviderServer")
            Prelude.<*> (x Prelude..: "staticKeyValue")
      )

instance Prelude.Hashable StaticKeySettings

instance Prelude.NFData StaticKeySettings

instance Prelude.ToJSON StaticKeySettings where
  toJSON StaticKeySettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("keyProviderServer" Prelude..=)
              Prelude.<$> keyProviderServer,
            Prelude.Just
              ("staticKeyValue" Prelude..= staticKeyValue)
          ]
      )
