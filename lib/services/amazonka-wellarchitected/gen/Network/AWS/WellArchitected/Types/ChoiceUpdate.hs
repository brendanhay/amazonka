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
-- Module      : Network.AWS.WellArchitected.Types.ChoiceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.ChoiceUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WellArchitected.Types.ChoiceReason
import Network.AWS.WellArchitected.Types.ChoiceStatus

-- | A list of choices to be updated.
--
-- /See:/ 'newChoiceUpdate' smart constructor.
data ChoiceUpdate = ChoiceUpdate'
  { -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe ChoiceReason,
    -- | The notes associated with a choice.
    notes :: Prelude.Maybe Prelude.Text,
    -- | The status of a choice.
    status :: ChoiceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChoiceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'choiceUpdate_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'notes', 'choiceUpdate_notes' - The notes associated with a choice.
--
-- 'status', 'choiceUpdate_status' - The status of a choice.
newChoiceUpdate ::
  -- | 'status'
  ChoiceStatus ->
  ChoiceUpdate
newChoiceUpdate pStatus_ =
  ChoiceUpdate'
    { reason = Prelude.Nothing,
      notes = Prelude.Nothing,
      status = pStatus_
    }

-- | The reason why a choice is non-applicable to a question in your
-- workload.
choiceUpdate_reason :: Lens.Lens' ChoiceUpdate (Prelude.Maybe ChoiceReason)
choiceUpdate_reason = Lens.lens (\ChoiceUpdate' {reason} -> reason) (\s@ChoiceUpdate' {} a -> s {reason = a} :: ChoiceUpdate)

-- | The notes associated with a choice.
choiceUpdate_notes :: Lens.Lens' ChoiceUpdate (Prelude.Maybe Prelude.Text)
choiceUpdate_notes = Lens.lens (\ChoiceUpdate' {notes} -> notes) (\s@ChoiceUpdate' {} a -> s {notes = a} :: ChoiceUpdate)

-- | The status of a choice.
choiceUpdate_status :: Lens.Lens' ChoiceUpdate ChoiceStatus
choiceUpdate_status = Lens.lens (\ChoiceUpdate' {status} -> status) (\s@ChoiceUpdate' {} a -> s {status = a} :: ChoiceUpdate)

instance Prelude.Hashable ChoiceUpdate

instance Prelude.NFData ChoiceUpdate

instance Core.ToJSON ChoiceUpdate where
  toJSON ChoiceUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Reason" Core..=) Prelude.<$> reason,
            ("Notes" Core..=) Prelude.<$> notes,
            Prelude.Just ("Status" Core..= status)
          ]
      )
