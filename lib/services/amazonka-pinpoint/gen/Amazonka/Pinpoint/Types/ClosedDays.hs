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
-- Module      : Amazonka.Pinpoint.Types.ClosedDays
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ClosedDays where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ClosedDaysRule
import qualified Amazonka.Prelude as Prelude

-- | The time when journey will stop sending messages.
--
-- /See:/ 'newClosedDays' smart constructor.
data ClosedDays = ClosedDays'
  { -- | Rules for Custom Channel.
    custom :: Prelude.Maybe [ClosedDaysRule],
    -- | Rules for Email Channel.
    email :: Prelude.Maybe [ClosedDaysRule],
    -- | Rules for Push Channel.
    push :: Prelude.Maybe [ClosedDaysRule],
    -- | Rules for SMS Channel.
    sms :: Prelude.Maybe [ClosedDaysRule],
    -- | Rules for Voice Channel.
    voice :: Prelude.Maybe [ClosedDaysRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClosedDays' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'custom', 'closedDays_custom' - Rules for Custom Channel.
--
-- 'email', 'closedDays_email' - Rules for Email Channel.
--
-- 'push', 'closedDays_push' - Rules for Push Channel.
--
-- 'sms', 'closedDays_sms' - Rules for SMS Channel.
--
-- 'voice', 'closedDays_voice' - Rules for Voice Channel.
newClosedDays ::
  ClosedDays
newClosedDays =
  ClosedDays'
    { custom = Prelude.Nothing,
      email = Prelude.Nothing,
      push = Prelude.Nothing,
      sms = Prelude.Nothing,
      voice = Prelude.Nothing
    }

-- | Rules for Custom Channel.
closedDays_custom :: Lens.Lens' ClosedDays (Prelude.Maybe [ClosedDaysRule])
closedDays_custom = Lens.lens (\ClosedDays' {custom} -> custom) (\s@ClosedDays' {} a -> s {custom = a} :: ClosedDays) Prelude.. Lens.mapping Lens.coerced

-- | Rules for Email Channel.
closedDays_email :: Lens.Lens' ClosedDays (Prelude.Maybe [ClosedDaysRule])
closedDays_email = Lens.lens (\ClosedDays' {email} -> email) (\s@ClosedDays' {} a -> s {email = a} :: ClosedDays) Prelude.. Lens.mapping Lens.coerced

-- | Rules for Push Channel.
closedDays_push :: Lens.Lens' ClosedDays (Prelude.Maybe [ClosedDaysRule])
closedDays_push = Lens.lens (\ClosedDays' {push} -> push) (\s@ClosedDays' {} a -> s {push = a} :: ClosedDays) Prelude.. Lens.mapping Lens.coerced

-- | Rules for SMS Channel.
closedDays_sms :: Lens.Lens' ClosedDays (Prelude.Maybe [ClosedDaysRule])
closedDays_sms = Lens.lens (\ClosedDays' {sms} -> sms) (\s@ClosedDays' {} a -> s {sms = a} :: ClosedDays) Prelude.. Lens.mapping Lens.coerced

-- | Rules for Voice Channel.
closedDays_voice :: Lens.Lens' ClosedDays (Prelude.Maybe [ClosedDaysRule])
closedDays_voice = Lens.lens (\ClosedDays' {voice} -> voice) (\s@ClosedDays' {} a -> s {voice = a} :: ClosedDays) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ClosedDays where
  parseJSON =
    Data.withObject
      "ClosedDays"
      ( \x ->
          ClosedDays'
            Prelude.<$> (x Data..:? "CUSTOM" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EMAIL" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PUSH" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SMS" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VOICE" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ClosedDays where
  hashWithSalt _salt ClosedDays' {..} =
    _salt
      `Prelude.hashWithSalt` custom
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` push
      `Prelude.hashWithSalt` sms
      `Prelude.hashWithSalt` voice

instance Prelude.NFData ClosedDays where
  rnf ClosedDays' {..} =
    Prelude.rnf custom `Prelude.seq`
      Prelude.rnf email `Prelude.seq`
        Prelude.rnf push `Prelude.seq`
          Prelude.rnf sms `Prelude.seq`
            Prelude.rnf voice

instance Data.ToJSON ClosedDays where
  toJSON ClosedDays' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CUSTOM" Data..=) Prelude.<$> custom,
            ("EMAIL" Data..=) Prelude.<$> email,
            ("PUSH" Data..=) Prelude.<$> push,
            ("SMS" Data..=) Prelude.<$> sms,
            ("VOICE" Data..=) Prelude.<$> voice
          ]
      )
