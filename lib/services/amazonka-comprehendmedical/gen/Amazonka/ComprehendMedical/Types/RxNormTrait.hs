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
-- Module      : Amazonka.ComprehendMedical.Types.RxNormTrait
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.RxNormTrait where

import Amazonka.ComprehendMedical.Types.RxNormTraitName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The contextual information for the entity. InferRxNorm recognizes the
-- trait @NEGATION@, which is any indication that the patient is not taking
-- a medication.
--
-- /See:/ 'newRxNormTrait' smart constructor.
data RxNormTrait = RxNormTrait'
  { -- | Provides a name or contextual description about the trait.
    name :: Prelude.Maybe RxNormTraitName,
    -- | The level of confidence that Amazon Comprehend Medical has in the
    -- accuracy of the detected trait.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RxNormTrait' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'rxNormTrait_name' - Provides a name or contextual description about the trait.
--
-- 'score', 'rxNormTrait_score' - The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected trait.
newRxNormTrait ::
  RxNormTrait
newRxNormTrait =
  RxNormTrait'
    { name = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | Provides a name or contextual description about the trait.
rxNormTrait_name :: Lens.Lens' RxNormTrait (Prelude.Maybe RxNormTraitName)
rxNormTrait_name = Lens.lens (\RxNormTrait' {name} -> name) (\s@RxNormTrait' {} a -> s {name = a} :: RxNormTrait)

-- | The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected trait.
rxNormTrait_score :: Lens.Lens' RxNormTrait (Prelude.Maybe Prelude.Double)
rxNormTrait_score = Lens.lens (\RxNormTrait' {score} -> score) (\s@RxNormTrait' {} a -> s {score = a} :: RxNormTrait)

instance Core.FromJSON RxNormTrait where
  parseJSON =
    Core.withObject
      "RxNormTrait"
      ( \x ->
          RxNormTrait'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Score")
      )

instance Prelude.Hashable RxNormTrait where
  hashWithSalt _salt RxNormTrait' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` score

instance Prelude.NFData RxNormTrait where
  rnf RxNormTrait' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf score
