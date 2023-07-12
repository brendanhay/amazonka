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
-- Module      : Amazonka.AmplifyBackend.Types.SmsSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.SmsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | SMS settings for authentication.
--
-- /See:/ 'newSmsSettings' smart constructor.
data SmsSettings = SmsSettings'
  { -- | The contents of the SMS message.
    smsMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SmsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'smsMessage', 'smsSettings_smsMessage' - The contents of the SMS message.
newSmsSettings ::
  SmsSettings
newSmsSettings =
  SmsSettings' {smsMessage = Prelude.Nothing}

-- | The contents of the SMS message.
smsSettings_smsMessage :: Lens.Lens' SmsSettings (Prelude.Maybe Prelude.Text)
smsSettings_smsMessage = Lens.lens (\SmsSettings' {smsMessage} -> smsMessage) (\s@SmsSettings' {} a -> s {smsMessage = a} :: SmsSettings)

instance Data.FromJSON SmsSettings where
  parseJSON =
    Data.withObject
      "SmsSettings"
      ( \x ->
          SmsSettings' Prelude.<$> (x Data..:? "smsMessage")
      )

instance Prelude.Hashable SmsSettings where
  hashWithSalt _salt SmsSettings' {..} =
    _salt `Prelude.hashWithSalt` smsMessage

instance Prelude.NFData SmsSettings where
  rnf SmsSettings' {..} = Prelude.rnf smsMessage

instance Data.ToJSON SmsSettings where
  toJSON SmsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("smsMessage" Data..=) Prelude.<$> smsMessage]
      )
