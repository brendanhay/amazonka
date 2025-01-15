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
-- Module      : Amazonka.LexV2Models.Types.DialogAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DialogAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.DialogActionType
import qualified Amazonka.Prelude as Prelude

-- | Defines the action that the bot executes at runtime when the
-- conversation reaches this step.
--
-- /See:/ 'newDialogAction' smart constructor.
data DialogAction = DialogAction'
  { -- | If the dialog action is @ElicitSlot@, defines the slot to elicit from
    -- the user.
    slotToElicit :: Prelude.Maybe Prelude.Text,
    -- | When true the next message for the intent is not used.
    suppressNextMessage :: Prelude.Maybe Prelude.Bool,
    -- | The action that the bot should execute.
    type' :: DialogActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DialogAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slotToElicit', 'dialogAction_slotToElicit' - If the dialog action is @ElicitSlot@, defines the slot to elicit from
-- the user.
--
-- 'suppressNextMessage', 'dialogAction_suppressNextMessage' - When true the next message for the intent is not used.
--
-- 'type'', 'dialogAction_type' - The action that the bot should execute.
newDialogAction ::
  -- | 'type''
  DialogActionType ->
  DialogAction
newDialogAction pType_ =
  DialogAction'
    { slotToElicit = Prelude.Nothing,
      suppressNextMessage = Prelude.Nothing,
      type' = pType_
    }

-- | If the dialog action is @ElicitSlot@, defines the slot to elicit from
-- the user.
dialogAction_slotToElicit :: Lens.Lens' DialogAction (Prelude.Maybe Prelude.Text)
dialogAction_slotToElicit = Lens.lens (\DialogAction' {slotToElicit} -> slotToElicit) (\s@DialogAction' {} a -> s {slotToElicit = a} :: DialogAction)

-- | When true the next message for the intent is not used.
dialogAction_suppressNextMessage :: Lens.Lens' DialogAction (Prelude.Maybe Prelude.Bool)
dialogAction_suppressNextMessage = Lens.lens (\DialogAction' {suppressNextMessage} -> suppressNextMessage) (\s@DialogAction' {} a -> s {suppressNextMessage = a} :: DialogAction)

-- | The action that the bot should execute.
dialogAction_type :: Lens.Lens' DialogAction DialogActionType
dialogAction_type = Lens.lens (\DialogAction' {type'} -> type') (\s@DialogAction' {} a -> s {type' = a} :: DialogAction)

instance Data.FromJSON DialogAction where
  parseJSON =
    Data.withObject
      "DialogAction"
      ( \x ->
          DialogAction'
            Prelude.<$> (x Data..:? "slotToElicit")
            Prelude.<*> (x Data..:? "suppressNextMessage")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable DialogAction where
  hashWithSalt _salt DialogAction' {..} =
    _salt
      `Prelude.hashWithSalt` slotToElicit
      `Prelude.hashWithSalt` suppressNextMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DialogAction where
  rnf DialogAction' {..} =
    Prelude.rnf slotToElicit `Prelude.seq`
      Prelude.rnf suppressNextMessage `Prelude.seq`
        Prelude.rnf type'

instance Data.ToJSON DialogAction where
  toJSON DialogAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("slotToElicit" Data..=) Prelude.<$> slotToElicit,
            ("suppressNextMessage" Data..=)
              Prelude.<$> suppressNextMessage,
            Prelude.Just ("type" Data..= type')
          ]
      )
