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
-- Module      : Amazonka.WellArchitected.Types.ChoiceUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ChoiceReason
import Amazonka.WellArchitected.Types.ChoiceStatus

-- | A list of choices to be updated.
--
-- /See:/ 'newChoiceUpdate' smart constructor.
data ChoiceUpdate = ChoiceUpdate'
  { -- | The notes associated with a choice.
    notes :: Prelude.Maybe Prelude.Text,
    -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe ChoiceReason,
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
-- 'notes', 'choiceUpdate_notes' - The notes associated with a choice.
--
-- 'reason', 'choiceUpdate_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'status', 'choiceUpdate_status' - The status of a choice.
newChoiceUpdate ::
  -- | 'status'
  ChoiceStatus ->
  ChoiceUpdate
newChoiceUpdate pStatus_ =
  ChoiceUpdate'
    { notes = Prelude.Nothing,
      reason = Prelude.Nothing,
      status = pStatus_
    }

-- | The notes associated with a choice.
choiceUpdate_notes :: Lens.Lens' ChoiceUpdate (Prelude.Maybe Prelude.Text)
choiceUpdate_notes = Lens.lens (\ChoiceUpdate' {notes} -> notes) (\s@ChoiceUpdate' {} a -> s {notes = a} :: ChoiceUpdate)

-- | The reason why a choice is non-applicable to a question in your
-- workload.
choiceUpdate_reason :: Lens.Lens' ChoiceUpdate (Prelude.Maybe ChoiceReason)
choiceUpdate_reason = Lens.lens (\ChoiceUpdate' {reason} -> reason) (\s@ChoiceUpdate' {} a -> s {reason = a} :: ChoiceUpdate)

-- | The status of a choice.
choiceUpdate_status :: Lens.Lens' ChoiceUpdate ChoiceStatus
choiceUpdate_status = Lens.lens (\ChoiceUpdate' {status} -> status) (\s@ChoiceUpdate' {} a -> s {status = a} :: ChoiceUpdate)

instance Prelude.Hashable ChoiceUpdate where
  hashWithSalt _salt ChoiceUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChoiceUpdate where
  rnf ChoiceUpdate' {..} =
    Prelude.rnf notes
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ChoiceUpdate where
  toJSON ChoiceUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Notes" Data..=) Prelude.<$> notes,
            ("Reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("Status" Data..= status)
          ]
      )
