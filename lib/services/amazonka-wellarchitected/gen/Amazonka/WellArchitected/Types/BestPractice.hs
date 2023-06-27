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
-- Module      : Amazonka.WellArchitected.Types.BestPractice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.BestPractice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A best practice, or question choice, that has been identified as a risk
-- in this question.
--
-- /See:/ 'newBestPractice' smart constructor.
data BestPractice = BestPractice'
  { choiceId :: Prelude.Maybe Prelude.Text,
    choiceTitle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BestPractice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choiceId', 'bestPractice_choiceId' - Undocumented member.
--
-- 'choiceTitle', 'bestPractice_choiceTitle' - Undocumented member.
newBestPractice ::
  BestPractice
newBestPractice =
  BestPractice'
    { choiceId = Prelude.Nothing,
      choiceTitle = Prelude.Nothing
    }

-- | Undocumented member.
bestPractice_choiceId :: Lens.Lens' BestPractice (Prelude.Maybe Prelude.Text)
bestPractice_choiceId = Lens.lens (\BestPractice' {choiceId} -> choiceId) (\s@BestPractice' {} a -> s {choiceId = a} :: BestPractice)

-- | Undocumented member.
bestPractice_choiceTitle :: Lens.Lens' BestPractice (Prelude.Maybe Prelude.Text)
bestPractice_choiceTitle = Lens.lens (\BestPractice' {choiceTitle} -> choiceTitle) (\s@BestPractice' {} a -> s {choiceTitle = a} :: BestPractice)

instance Data.FromJSON BestPractice where
  parseJSON =
    Data.withObject
      "BestPractice"
      ( \x ->
          BestPractice'
            Prelude.<$> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "ChoiceTitle")
      )

instance Prelude.Hashable BestPractice where
  hashWithSalt _salt BestPractice' {..} =
    _salt
      `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` choiceTitle

instance Prelude.NFData BestPractice where
  rnf BestPractice' {..} =
    Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf choiceTitle
