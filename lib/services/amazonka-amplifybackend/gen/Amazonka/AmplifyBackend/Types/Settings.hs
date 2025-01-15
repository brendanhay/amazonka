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
-- Module      : Amazonka.AmplifyBackend.Types.Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.Settings where

import Amazonka.AmplifyBackend.Types.MfaTypesElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings of your MFA configuration for the backend of your Amplify
-- project.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | The supported MFA types.
    mfaTypes :: Prelude.Maybe [MfaTypesElement],
    -- | The body of the SMS message.
    smsMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaTypes', 'settings_mfaTypes' - The supported MFA types.
--
-- 'smsMessage', 'settings_smsMessage' - The body of the SMS message.
newSettings ::
  Settings
newSettings =
  Settings'
    { mfaTypes = Prelude.Nothing,
      smsMessage = Prelude.Nothing
    }

-- | The supported MFA types.
settings_mfaTypes :: Lens.Lens' Settings (Prelude.Maybe [MfaTypesElement])
settings_mfaTypes = Lens.lens (\Settings' {mfaTypes} -> mfaTypes) (\s@Settings' {} a -> s {mfaTypes = a} :: Settings) Prelude.. Lens.mapping Lens.coerced

-- | The body of the SMS message.
settings_smsMessage :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_smsMessage = Lens.lens (\Settings' {smsMessage} -> smsMessage) (\s@Settings' {} a -> s {smsMessage = a} :: Settings)

instance Data.FromJSON Settings where
  parseJSON =
    Data.withObject
      "Settings"
      ( \x ->
          Settings'
            Prelude.<$> (x Data..:? "mfaTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "smsMessage")
      )

instance Prelude.Hashable Settings where
  hashWithSalt _salt Settings' {..} =
    _salt
      `Prelude.hashWithSalt` mfaTypes
      `Prelude.hashWithSalt` smsMessage

instance Prelude.NFData Settings where
  rnf Settings' {..} =
    Prelude.rnf mfaTypes `Prelude.seq`
      Prelude.rnf smsMessage

instance Data.ToJSON Settings where
  toJSON Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mfaTypes" Data..=) Prelude.<$> mfaTypes,
            ("smsMessage" Data..=) Prelude.<$> smsMessage
          ]
      )
