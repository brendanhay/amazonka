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
-- Module      : Amazonka.WellArchitected.Types.ChoiceAnswer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceAnswer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ChoiceReason
import Amazonka.WellArchitected.Types.ChoiceStatus

-- | A choice that has been answered on a question in your workload.
--
-- /See:/ 'newChoiceAnswer' smart constructor.
data ChoiceAnswer = ChoiceAnswer'
  { choiceId :: Prelude.Maybe Prelude.Text,
    -- | The notes associated with a choice.
    notes :: Prelude.Maybe Prelude.Text,
    -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe ChoiceReason,
    -- | The status of a choice.
    status :: Prelude.Maybe ChoiceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChoiceAnswer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choiceId', 'choiceAnswer_choiceId' - Undocumented member.
--
-- 'notes', 'choiceAnswer_notes' - The notes associated with a choice.
--
-- 'reason', 'choiceAnswer_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'status', 'choiceAnswer_status' - The status of a choice.
newChoiceAnswer ::
  ChoiceAnswer
newChoiceAnswer =
  ChoiceAnswer'
    { choiceId = Prelude.Nothing,
      notes = Prelude.Nothing,
      reason = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Undocumented member.
choiceAnswer_choiceId :: Lens.Lens' ChoiceAnswer (Prelude.Maybe Prelude.Text)
choiceAnswer_choiceId = Lens.lens (\ChoiceAnswer' {choiceId} -> choiceId) (\s@ChoiceAnswer' {} a -> s {choiceId = a} :: ChoiceAnswer)

-- | The notes associated with a choice.
choiceAnswer_notes :: Lens.Lens' ChoiceAnswer (Prelude.Maybe Prelude.Text)
choiceAnswer_notes = Lens.lens (\ChoiceAnswer' {notes} -> notes) (\s@ChoiceAnswer' {} a -> s {notes = a} :: ChoiceAnswer)

-- | The reason why a choice is non-applicable to a question in your
-- workload.
choiceAnswer_reason :: Lens.Lens' ChoiceAnswer (Prelude.Maybe ChoiceReason)
choiceAnswer_reason = Lens.lens (\ChoiceAnswer' {reason} -> reason) (\s@ChoiceAnswer' {} a -> s {reason = a} :: ChoiceAnswer)

-- | The status of a choice.
choiceAnswer_status :: Lens.Lens' ChoiceAnswer (Prelude.Maybe ChoiceStatus)
choiceAnswer_status = Lens.lens (\ChoiceAnswer' {status} -> status) (\s@ChoiceAnswer' {} a -> s {status = a} :: ChoiceAnswer)

instance Data.FromJSON ChoiceAnswer where
  parseJSON =
    Data.withObject
      "ChoiceAnswer"
      ( \x ->
          ChoiceAnswer'
            Prelude.<$> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "Notes")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ChoiceAnswer where
  hashWithSalt _salt ChoiceAnswer' {..} =
    _salt
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChoiceAnswer where
  rnf ChoiceAnswer' {..} =
    Prelude.rnf choiceId `Prelude.seq`
      Prelude.rnf notes `Prelude.seq`
        Prelude.rnf reason `Prelude.seq`
          Prelude.rnf status
