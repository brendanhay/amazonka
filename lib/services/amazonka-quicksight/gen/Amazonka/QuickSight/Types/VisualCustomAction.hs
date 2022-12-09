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
-- Module      : Amazonka.QuickSight.Types.VisualCustomAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualCustomAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.VisualCustomActionOperation
import Amazonka.QuickSight.Types.VisualCustomActionTrigger
import Amazonka.QuickSight.Types.WidgetStatus

-- | A custom action defined on a visual.
--
-- /See:/ 'newVisualCustomAction' smart constructor.
data VisualCustomAction = VisualCustomAction'
  { -- | The status of the @VisualCustomAction@.
    status :: Prelude.Maybe WidgetStatus,
    -- | The ID of the @VisualCustomAction@.
    customActionId :: Prelude.Text,
    -- | The name of the @VisualCustomAction@.
    name :: Prelude.Text,
    -- | The trigger of the @VisualCustomAction@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @DATA_POINT_CLICK@: Initiates a custom action by a left pointer
    --     click on a data point.
    --
    -- -   @DATA_POINT_MENU@: Initiates a custom action by right pointer click
    --     from the menu.
    trigger :: VisualCustomActionTrigger,
    -- | A list of @VisualCustomActionOperations@.
    --
    -- This is a union type structure. For this structure to be valid, only one
    -- of the attributes can be defined.
    actionOperations :: Prelude.NonEmpty VisualCustomActionOperation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualCustomAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'visualCustomAction_status' - The status of the @VisualCustomAction@.
--
-- 'customActionId', 'visualCustomAction_customActionId' - The ID of the @VisualCustomAction@.
--
-- 'name', 'visualCustomAction_name' - The name of the @VisualCustomAction@.
--
-- 'trigger', 'visualCustomAction_trigger' - The trigger of the @VisualCustomAction@.
--
-- Valid values are defined as follows:
--
-- -   @DATA_POINT_CLICK@: Initiates a custom action by a left pointer
--     click on a data point.
--
-- -   @DATA_POINT_MENU@: Initiates a custom action by right pointer click
--     from the menu.
--
-- 'actionOperations', 'visualCustomAction_actionOperations' - A list of @VisualCustomActionOperations@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
newVisualCustomAction ::
  -- | 'customActionId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'trigger'
  VisualCustomActionTrigger ->
  -- | 'actionOperations'
  Prelude.NonEmpty VisualCustomActionOperation ->
  VisualCustomAction
newVisualCustomAction
  pCustomActionId_
  pName_
  pTrigger_
  pActionOperations_ =
    VisualCustomAction'
      { status = Prelude.Nothing,
        customActionId = pCustomActionId_,
        name = pName_,
        trigger = pTrigger_,
        actionOperations =
          Lens.coerced Lens.# pActionOperations_
      }

-- | The status of the @VisualCustomAction@.
visualCustomAction_status :: Lens.Lens' VisualCustomAction (Prelude.Maybe WidgetStatus)
visualCustomAction_status = Lens.lens (\VisualCustomAction' {status} -> status) (\s@VisualCustomAction' {} a -> s {status = a} :: VisualCustomAction)

-- | The ID of the @VisualCustomAction@.
visualCustomAction_customActionId :: Lens.Lens' VisualCustomAction Prelude.Text
visualCustomAction_customActionId = Lens.lens (\VisualCustomAction' {customActionId} -> customActionId) (\s@VisualCustomAction' {} a -> s {customActionId = a} :: VisualCustomAction)

-- | The name of the @VisualCustomAction@.
visualCustomAction_name :: Lens.Lens' VisualCustomAction Prelude.Text
visualCustomAction_name = Lens.lens (\VisualCustomAction' {name} -> name) (\s@VisualCustomAction' {} a -> s {name = a} :: VisualCustomAction)

-- | The trigger of the @VisualCustomAction@.
--
-- Valid values are defined as follows:
--
-- -   @DATA_POINT_CLICK@: Initiates a custom action by a left pointer
--     click on a data point.
--
-- -   @DATA_POINT_MENU@: Initiates a custom action by right pointer click
--     from the menu.
visualCustomAction_trigger :: Lens.Lens' VisualCustomAction VisualCustomActionTrigger
visualCustomAction_trigger = Lens.lens (\VisualCustomAction' {trigger} -> trigger) (\s@VisualCustomAction' {} a -> s {trigger = a} :: VisualCustomAction)

-- | A list of @VisualCustomActionOperations@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
visualCustomAction_actionOperations :: Lens.Lens' VisualCustomAction (Prelude.NonEmpty VisualCustomActionOperation)
visualCustomAction_actionOperations = Lens.lens (\VisualCustomAction' {actionOperations} -> actionOperations) (\s@VisualCustomAction' {} a -> s {actionOperations = a} :: VisualCustomAction) Prelude.. Lens.coerced

instance Data.FromJSON VisualCustomAction where
  parseJSON =
    Data.withObject
      "VisualCustomAction"
      ( \x ->
          VisualCustomAction'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..: "CustomActionId")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Trigger")
            Prelude.<*> (x Data..: "ActionOperations")
      )

instance Prelude.Hashable VisualCustomAction where
  hashWithSalt _salt VisualCustomAction' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` customActionId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` trigger
      `Prelude.hashWithSalt` actionOperations

instance Prelude.NFData VisualCustomAction where
  rnf VisualCustomAction' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf customActionId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf trigger
      `Prelude.seq` Prelude.rnf actionOperations

instance Data.ToJSON VisualCustomAction where
  toJSON VisualCustomAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Status" Data..=) Prelude.<$> status,
            Prelude.Just
              ("CustomActionId" Data..= customActionId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Trigger" Data..= trigger),
            Prelude.Just
              ("ActionOperations" Data..= actionOperations)
          ]
      )
