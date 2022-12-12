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
-- Module      : Amazonka.Pinpoint.Types.OpenHours
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.OpenHours where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.DayOfWeek
import Amazonka.Pinpoint.Types.OpenHoursRule
import qualified Amazonka.Prelude as Prelude

-- | The time when journey allow to send messages. QuietTime should be
-- configured first and SendingSchedule should be set to true.
--
-- /See:/ 'newOpenHours' smart constructor.
data OpenHours = OpenHours'
  { -- | Rules for Custom Channel.
    custom :: Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]),
    -- | Rules for Email Channel.
    email :: Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]),
    -- | Rules for Push Channel.
    push :: Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]),
    -- | Rules for SMS Channel.
    sms :: Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]),
    -- | Rules for Voice Channel.
    voice :: Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenHours' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'custom', 'openHours_custom' - Rules for Custom Channel.
--
-- 'email', 'openHours_email' - Rules for Email Channel.
--
-- 'push', 'openHours_push' - Rules for Push Channel.
--
-- 'sms', 'openHours_sms' - Rules for SMS Channel.
--
-- 'voice', 'openHours_voice' - Rules for Voice Channel.
newOpenHours ::
  OpenHours
newOpenHours =
  OpenHours'
    { custom = Prelude.Nothing,
      email = Prelude.Nothing,
      push = Prelude.Nothing,
      sms = Prelude.Nothing,
      voice = Prelude.Nothing
    }

-- | Rules for Custom Channel.
openHours_custom :: Lens.Lens' OpenHours (Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]))
openHours_custom = Lens.lens (\OpenHours' {custom} -> custom) (\s@OpenHours' {} a -> s {custom = a} :: OpenHours) Prelude.. Lens.mapping Lens.coerced

-- | Rules for Email Channel.
openHours_email :: Lens.Lens' OpenHours (Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]))
openHours_email = Lens.lens (\OpenHours' {email} -> email) (\s@OpenHours' {} a -> s {email = a} :: OpenHours) Prelude.. Lens.mapping Lens.coerced

-- | Rules for Push Channel.
openHours_push :: Lens.Lens' OpenHours (Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]))
openHours_push = Lens.lens (\OpenHours' {push} -> push) (\s@OpenHours' {} a -> s {push = a} :: OpenHours) Prelude.. Lens.mapping Lens.coerced

-- | Rules for SMS Channel.
openHours_sms :: Lens.Lens' OpenHours (Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]))
openHours_sms = Lens.lens (\OpenHours' {sms} -> sms) (\s@OpenHours' {} a -> s {sms = a} :: OpenHours) Prelude.. Lens.mapping Lens.coerced

-- | Rules for Voice Channel.
openHours_voice :: Lens.Lens' OpenHours (Prelude.Maybe (Prelude.HashMap DayOfWeek [OpenHoursRule]))
openHours_voice = Lens.lens (\OpenHours' {voice} -> voice) (\s@OpenHours' {} a -> s {voice = a} :: OpenHours) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OpenHours where
  parseJSON =
    Data.withObject
      "OpenHours"
      ( \x ->
          OpenHours'
            Prelude.<$> (x Data..:? "CUSTOM" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EMAIL" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PUSH" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SMS" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VOICE" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OpenHours where
  hashWithSalt _salt OpenHours' {..} =
    _salt `Prelude.hashWithSalt` custom
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` push
      `Prelude.hashWithSalt` sms
      `Prelude.hashWithSalt` voice

instance Prelude.NFData OpenHours where
  rnf OpenHours' {..} =
    Prelude.rnf custom
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf push
      `Prelude.seq` Prelude.rnf sms
      `Prelude.seq` Prelude.rnf voice

instance Data.ToJSON OpenHours where
  toJSON OpenHours' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CUSTOM" Data..=) Prelude.<$> custom,
            ("EMAIL" Data..=) Prelude.<$> email,
            ("PUSH" Data..=) Prelude.<$> push,
            ("SMS" Data..=) Prelude.<$> sms,
            ("VOICE" Data..=) Prelude.<$> voice
          ]
      )
