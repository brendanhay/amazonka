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
-- Module      : Amazonka.LexV2Models.Types.RuntimeHints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.RuntimeHints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.RuntimeHintDetails
import qualified Amazonka.Prelude as Prelude

-- | You can provide Amazon Lex with hints to the phrases that a customer is
-- likely to use for a slot. When a slot with hints is resolved, the
-- phrases in the runtime hints are preferred in the resolution. You can
-- provide hints for a maximum of 100 intents. You can provide a maximum of
-- 100 slots.
--
-- Before you can use runtime hints with an existing bot, you must first
-- rebuild the bot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/using-hints.html Using runtime hints to improve recognition of slot values>.
--
-- /See:/ 'newRuntimeHints' smart constructor.
data RuntimeHints = RuntimeHints'
  { -- | A list of the slots in the intent that should have runtime hints added,
    -- and the phrases that should be added for each slot.
    --
    -- The first level of the @slotHints@ map is the name of the intent. The
    -- second level is the name of the slot within the intent. For more
    -- information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/using-hints.html Using hints to improve accuracy>.
    --
    -- The intent name and slot name must exist.
    slotHints :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text RuntimeHintDetails))
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeHints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotHints', 'runtimeHints_slotHints' - A list of the slots in the intent that should have runtime hints added,
-- and the phrases that should be added for each slot.
--
-- The first level of the @slotHints@ map is the name of the intent. The
-- second level is the name of the slot within the intent. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/using-hints.html Using hints to improve accuracy>.
--
-- The intent name and slot name must exist.
newRuntimeHints ::
  RuntimeHints
newRuntimeHints =
  RuntimeHints' {slotHints = Prelude.Nothing}

-- | A list of the slots in the intent that should have runtime hints added,
-- and the phrases that should be added for each slot.
--
-- The first level of the @slotHints@ map is the name of the intent. The
-- second level is the name of the slot within the intent. For more
-- information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/using-hints.html Using hints to improve accuracy>.
--
-- The intent name and slot name must exist.
runtimeHints_slotHints :: Lens.Lens' RuntimeHints (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text RuntimeHintDetails)))
runtimeHints_slotHints = Lens.lens (\RuntimeHints' {slotHints} -> slotHints) (\s@RuntimeHints' {} a -> s {slotHints = a} :: RuntimeHints) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RuntimeHints where
  parseJSON =
    Data.withObject
      "RuntimeHints"
      ( \x ->
          RuntimeHints'
            Prelude.<$> (x Data..:? "slotHints" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RuntimeHints where
  hashWithSalt _salt RuntimeHints' {..} =
    _salt `Prelude.hashWithSalt` slotHints

instance Prelude.NFData RuntimeHints where
  rnf RuntimeHints' {..} = Prelude.rnf slotHints
