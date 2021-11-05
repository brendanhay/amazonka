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
-- Module      : Network.AWS.ComprehendMedical.Types.RxNormTrait
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComprehendMedical.Types.RxNormTrait where

import Network.AWS.ComprehendMedical.Types.RxNormTraitName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The contextual information for the entity. InferRxNorm recognizes the
-- trait @NEGATION@, which is any indication that the patient is not taking
-- a medication.
--
-- /See:/ 'newRxNormTrait' smart constructor.
data RxNormTrait = RxNormTrait'
  { -- | The level of confidence that Amazon Comprehend Medical has in the
    -- accuracy of the detected trait.
    score :: Prelude.Maybe Prelude.Double,
    -- | Provides a name or contextual description about the trait.
    name :: Prelude.Maybe RxNormTraitName
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
-- 'score', 'rxNormTrait_score' - The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected trait.
--
-- 'name', 'rxNormTrait_name' - Provides a name or contextual description about the trait.
newRxNormTrait ::
  RxNormTrait
newRxNormTrait =
  RxNormTrait'
    { score = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The level of confidence that Amazon Comprehend Medical has in the
-- accuracy of the detected trait.
rxNormTrait_score :: Lens.Lens' RxNormTrait (Prelude.Maybe Prelude.Double)
rxNormTrait_score = Lens.lens (\RxNormTrait' {score} -> score) (\s@RxNormTrait' {} a -> s {score = a} :: RxNormTrait)

-- | Provides a name or contextual description about the trait.
rxNormTrait_name :: Lens.Lens' RxNormTrait (Prelude.Maybe RxNormTraitName)
rxNormTrait_name = Lens.lens (\RxNormTrait' {name} -> name) (\s@RxNormTrait' {} a -> s {name = a} :: RxNormTrait)

instance Core.FromJSON RxNormTrait where
  parseJSON =
    Core.withObject
      "RxNormTrait"
      ( \x ->
          RxNormTrait'
            Prelude.<$> (x Core..:? "Score") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable RxNormTrait

instance Prelude.NFData RxNormTrait
