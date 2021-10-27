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
-- Module      : Network.AWS.LexV2Runtime.Types.Intent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.Intent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.ConfirmationState
import Network.AWS.LexV2Runtime.Types.IntentState
import Network.AWS.LexV2Runtime.Types.Slot
import qualified Network.AWS.Prelude as Prelude

-- | The current intent that Amazon Lex V2 is attempting to fulfill.
--
-- /See:/ 'newIntent' smart constructor.
data Intent = Intent'
  { -- | A map of all of the slots for the intent. The name of the slot maps to
    -- the value of the slot. If a slot has not been filled, the value is null.
    slots :: Prelude.Maybe (Prelude.HashMap Prelude.Text Slot),
    -- | Contains fulfillment information for the intent.
    state :: Prelude.Maybe IntentState,
    -- | Contains information about whether fulfillment of the intent has been
    -- confirmed.
    confirmationState :: Prelude.Maybe ConfirmationState,
    -- | The name of the intent.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Intent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slots', 'intent_slots' - A map of all of the slots for the intent. The name of the slot maps to
-- the value of the slot. If a slot has not been filled, the value is null.
--
-- 'state', 'intent_state' - Contains fulfillment information for the intent.
--
-- 'confirmationState', 'intent_confirmationState' - Contains information about whether fulfillment of the intent has been
-- confirmed.
--
-- 'name', 'intent_name' - The name of the intent.
newIntent ::
  -- | 'name'
  Prelude.Text ->
  Intent
newIntent pName_ =
  Intent'
    { slots = Prelude.Nothing,
      state = Prelude.Nothing,
      confirmationState = Prelude.Nothing,
      name = pName_
    }

-- | A map of all of the slots for the intent. The name of the slot maps to
-- the value of the slot. If a slot has not been filled, the value is null.
intent_slots :: Lens.Lens' Intent (Prelude.Maybe (Prelude.HashMap Prelude.Text Slot))
intent_slots = Lens.lens (\Intent' {slots} -> slots) (\s@Intent' {} a -> s {slots = a} :: Intent) Prelude.. Lens.mapping Lens.coerced

-- | Contains fulfillment information for the intent.
intent_state :: Lens.Lens' Intent (Prelude.Maybe IntentState)
intent_state = Lens.lens (\Intent' {state} -> state) (\s@Intent' {} a -> s {state = a} :: Intent)

-- | Contains information about whether fulfillment of the intent has been
-- confirmed.
intent_confirmationState :: Lens.Lens' Intent (Prelude.Maybe ConfirmationState)
intent_confirmationState = Lens.lens (\Intent' {confirmationState} -> confirmationState) (\s@Intent' {} a -> s {confirmationState = a} :: Intent)

-- | The name of the intent.
intent_name :: Lens.Lens' Intent Prelude.Text
intent_name = Lens.lens (\Intent' {name} -> name) (\s@Intent' {} a -> s {name = a} :: Intent)

instance Core.FromJSON Intent where
  parseJSON =
    Core.withObject
      "Intent"
      ( \x ->
          Intent'
            Prelude.<$> (x Core..:? "slots" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "confirmationState")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Intent

instance Prelude.NFData Intent

instance Core.ToJSON Intent where
  toJSON Intent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("slots" Core..=) Prelude.<$> slots,
            ("state" Core..=) Prelude.<$> state,
            ("confirmationState" Core..=)
              Prelude.<$> confirmationState,
            Prelude.Just ("name" Core..= name)
          ]
      )
