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
-- Module      : Amazonka.WellArchitected.Types.PillarDifference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.PillarDifference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.DifferenceStatus
import Amazonka.WellArchitected.Types.QuestionDifference

-- | A pillar difference return object.
--
-- /See:/ 'newPillarDifference' smart constructor.
data PillarDifference = PillarDifference'
  { -- | Indicates the type of change to the pillar.
    differenceStatus :: Prelude.Maybe DifferenceStatus,
    -- | List of question differences.
    questionDifferences :: Prelude.Maybe [QuestionDifference],
    pillarId :: Prelude.Maybe Prelude.Text,
    pillarName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PillarDifference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'differenceStatus', 'pillarDifference_differenceStatus' - Indicates the type of change to the pillar.
--
-- 'questionDifferences', 'pillarDifference_questionDifferences' - List of question differences.
--
-- 'pillarId', 'pillarDifference_pillarId' - Undocumented member.
--
-- 'pillarName', 'pillarDifference_pillarName' - Undocumented member.
newPillarDifference ::
  PillarDifference
newPillarDifference =
  PillarDifference'
    { differenceStatus =
        Prelude.Nothing,
      questionDifferences = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      pillarName = Prelude.Nothing
    }

-- | Indicates the type of change to the pillar.
pillarDifference_differenceStatus :: Lens.Lens' PillarDifference (Prelude.Maybe DifferenceStatus)
pillarDifference_differenceStatus = Lens.lens (\PillarDifference' {differenceStatus} -> differenceStatus) (\s@PillarDifference' {} a -> s {differenceStatus = a} :: PillarDifference)

-- | List of question differences.
pillarDifference_questionDifferences :: Lens.Lens' PillarDifference (Prelude.Maybe [QuestionDifference])
pillarDifference_questionDifferences = Lens.lens (\PillarDifference' {questionDifferences} -> questionDifferences) (\s@PillarDifference' {} a -> s {questionDifferences = a} :: PillarDifference) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
pillarDifference_pillarId :: Lens.Lens' PillarDifference (Prelude.Maybe Prelude.Text)
pillarDifference_pillarId = Lens.lens (\PillarDifference' {pillarId} -> pillarId) (\s@PillarDifference' {} a -> s {pillarId = a} :: PillarDifference)

-- | Undocumented member.
pillarDifference_pillarName :: Lens.Lens' PillarDifference (Prelude.Maybe Prelude.Text)
pillarDifference_pillarName = Lens.lens (\PillarDifference' {pillarName} -> pillarName) (\s@PillarDifference' {} a -> s {pillarName = a} :: PillarDifference)

instance Core.FromJSON PillarDifference where
  parseJSON =
    Core.withObject
      "PillarDifference"
      ( \x ->
          PillarDifference'
            Prelude.<$> (x Core..:? "DifferenceStatus")
            Prelude.<*> ( x Core..:? "QuestionDifferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PillarId")
            Prelude.<*> (x Core..:? "PillarName")
      )

instance Prelude.Hashable PillarDifference where
  hashWithSalt _salt PillarDifference' {..} =
    _salt `Prelude.hashWithSalt` differenceStatus
      `Prelude.hashWithSalt` questionDifferences
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` pillarName

instance Prelude.NFData PillarDifference where
  rnf PillarDifference' {..} =
    Prelude.rnf differenceStatus
      `Prelude.seq` Prelude.rnf questionDifferences
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf pillarName
