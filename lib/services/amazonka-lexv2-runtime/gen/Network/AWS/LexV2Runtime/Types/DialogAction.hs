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
-- Module      : Network.AWS.LexV2Runtime.Types.DialogAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.DialogAction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.DialogActionType
import qualified Network.AWS.Prelude as Prelude

-- | The next action that Amazon Lex V2 should take.
--
-- /See:/ 'newDialogAction' smart constructor.
data DialogAction = DialogAction'
  { -- | The name of the slot that should be elicited from the user.
    slotToElicit :: Prelude.Maybe Prelude.Text,
    -- | The next action that the bot should take in its interaction with the
    -- user. The possible values are:
    --
    -- -   @Close@ - Indicates that there will not be a response from the user.
    --     For example, the statement \"Your order has been placed\" does not
    --     require a response.
    --
    -- -   @ConfirmIntent@ - The next action is asking the user if the intent
    --     is complete and ready to be fulfilled. This is a yes\/no question
    --     such as \"Place the order?\"
    --
    -- -   @Delegate@ - The next action is determined by Amazon Lex V2.
    --
    -- -   @ElicitSlot@ - The next action is to elicit a slot value from the
    --     user.
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
-- 'slotToElicit', 'dialogAction_slotToElicit' - The name of the slot that should be elicited from the user.
--
-- 'type'', 'dialogAction_type' - The next action that the bot should take in its interaction with the
-- user. The possible values are:
--
-- -   @Close@ - Indicates that there will not be a response from the user.
--     For example, the statement \"Your order has been placed\" does not
--     require a response.
--
-- -   @ConfirmIntent@ - The next action is asking the user if the intent
--     is complete and ready to be fulfilled. This is a yes\/no question
--     such as \"Place the order?\"
--
-- -   @Delegate@ - The next action is determined by Amazon Lex V2.
--
-- -   @ElicitSlot@ - The next action is to elicit a slot value from the
--     user.
newDialogAction ::
  -- | 'type''
  DialogActionType ->
  DialogAction
newDialogAction pType_ =
  DialogAction'
    { slotToElicit = Prelude.Nothing,
      type' = pType_
    }

-- | The name of the slot that should be elicited from the user.
dialogAction_slotToElicit :: Lens.Lens' DialogAction (Prelude.Maybe Prelude.Text)
dialogAction_slotToElicit = Lens.lens (\DialogAction' {slotToElicit} -> slotToElicit) (\s@DialogAction' {} a -> s {slotToElicit = a} :: DialogAction)

-- | The next action that the bot should take in its interaction with the
-- user. The possible values are:
--
-- -   @Close@ - Indicates that there will not be a response from the user.
--     For example, the statement \"Your order has been placed\" does not
--     require a response.
--
-- -   @ConfirmIntent@ - The next action is asking the user if the intent
--     is complete and ready to be fulfilled. This is a yes\/no question
--     such as \"Place the order?\"
--
-- -   @Delegate@ - The next action is determined by Amazon Lex V2.
--
-- -   @ElicitSlot@ - The next action is to elicit a slot value from the
--     user.
dialogAction_type :: Lens.Lens' DialogAction DialogActionType
dialogAction_type = Lens.lens (\DialogAction' {type'} -> type') (\s@DialogAction' {} a -> s {type' = a} :: DialogAction)

instance Core.FromJSON DialogAction where
  parseJSON =
    Core.withObject
      "DialogAction"
      ( \x ->
          DialogAction'
            Prelude.<$> (x Core..:? "slotToElicit")
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable DialogAction

instance Prelude.NFData DialogAction

instance Core.ToJSON DialogAction where
  toJSON DialogAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("slotToElicit" Core..=) Prelude.<$> slotToElicit,
            Prelude.Just ("type" Core..= type')
          ]
      )
