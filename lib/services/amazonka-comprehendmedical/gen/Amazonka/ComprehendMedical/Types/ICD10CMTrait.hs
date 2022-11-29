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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMTrait
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMTrait where

import Amazonka.ComprehendMedical.Types.ICD10CMTraitName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contextual information for the entity. The traits recognized by
-- InferICD10CM are @DIAGNOSIS@, @SIGN@, @SYMPTOM@, and @NEGATION@.
--
-- /See:/ 'newICD10CMTrait' smart constructor.
data ICD10CMTrait = ICD10CMTrait'
  { -- | Provides a name or contextual description about the trait.
    name :: Prelude.Maybe ICD10CMTraitName,
    -- | The level of confidence that Comprehend Medical; has that the segment of
    -- text is correctly recognized as a trait.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ICD10CMTrait' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'iCD10CMTrait_name' - Provides a name or contextual description about the trait.
--
-- 'score', 'iCD10CMTrait_score' - The level of confidence that Comprehend Medical; has that the segment of
-- text is correctly recognized as a trait.
newICD10CMTrait ::
  ICD10CMTrait
newICD10CMTrait =
  ICD10CMTrait'
    { name = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | Provides a name or contextual description about the trait.
iCD10CMTrait_name :: Lens.Lens' ICD10CMTrait (Prelude.Maybe ICD10CMTraitName)
iCD10CMTrait_name = Lens.lens (\ICD10CMTrait' {name} -> name) (\s@ICD10CMTrait' {} a -> s {name = a} :: ICD10CMTrait)

-- | The level of confidence that Comprehend Medical; has that the segment of
-- text is correctly recognized as a trait.
iCD10CMTrait_score :: Lens.Lens' ICD10CMTrait (Prelude.Maybe Prelude.Double)
iCD10CMTrait_score = Lens.lens (\ICD10CMTrait' {score} -> score) (\s@ICD10CMTrait' {} a -> s {score = a} :: ICD10CMTrait)

instance Core.FromJSON ICD10CMTrait where
  parseJSON =
    Core.withObject
      "ICD10CMTrait"
      ( \x ->
          ICD10CMTrait'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Score")
      )

instance Prelude.Hashable ICD10CMTrait where
  hashWithSalt _salt ICD10CMTrait' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` score

instance Prelude.NFData ICD10CMTrait where
  rnf ICD10CMTrait' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf score
