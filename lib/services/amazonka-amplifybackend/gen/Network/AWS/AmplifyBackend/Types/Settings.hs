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
-- Module      : Network.AWS.AmplifyBackend.Types.Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types.Settings where

import Network.AWS.AmplifyBackend.Types.MfaTypesElement
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The settings of your MFA configuration for the backend of your Amplify
-- project.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | The body of the SMS message.
    smsMessage :: Prelude.Maybe Prelude.Text,
    -- | The supported MFA types.
    mfaTypes :: Prelude.Maybe [MfaTypesElement]
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
-- 'smsMessage', 'settings_smsMessage' - The body of the SMS message.
--
-- 'mfaTypes', 'settings_mfaTypes' - The supported MFA types.
newSettings ::
  Settings
newSettings =
  Settings'
    { smsMessage = Prelude.Nothing,
      mfaTypes = Prelude.Nothing
    }

-- | The body of the SMS message.
settings_smsMessage :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_smsMessage = Lens.lens (\Settings' {smsMessage} -> smsMessage) (\s@Settings' {} a -> s {smsMessage = a} :: Settings)

-- | The supported MFA types.
settings_mfaTypes :: Lens.Lens' Settings (Prelude.Maybe [MfaTypesElement])
settings_mfaTypes = Lens.lens (\Settings' {mfaTypes} -> mfaTypes) (\s@Settings' {} a -> s {mfaTypes = a} :: Settings) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Settings where
  parseJSON =
    Core.withObject
      "Settings"
      ( \x ->
          Settings'
            Prelude.<$> (x Core..:? "smsMessage")
            Prelude.<*> (x Core..:? "mfaTypes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Settings

instance Prelude.NFData Settings

instance Core.ToJSON Settings where
  toJSON Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("smsMessage" Core..=) Prelude.<$> smsMessage,
            ("mfaTypes" Core..=) Prelude.<$> mfaTypes
          ]
      )
