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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTTrait
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTTrait where

import Amazonka.ComprehendMedical.Types.SNOMEDCTTraitName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contextual information for an entity.
--
-- /See:/ 'newSNOMEDCTTrait' smart constructor.
data SNOMEDCTTrait = SNOMEDCTTrait'
  { -- | The name or contextual description of a detected trait.
    name :: Prelude.Maybe SNOMEDCTTraitName,
    -- | The level of confidence that Comprehend Medical has in the accuracy of a
    -- detected trait.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNOMEDCTTrait' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sNOMEDCTTrait_name' - The name or contextual description of a detected trait.
--
-- 'score', 'sNOMEDCTTrait_score' - The level of confidence that Comprehend Medical has in the accuracy of a
-- detected trait.
newSNOMEDCTTrait ::
  SNOMEDCTTrait
newSNOMEDCTTrait =
  SNOMEDCTTrait'
    { name = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The name or contextual description of a detected trait.
sNOMEDCTTrait_name :: Lens.Lens' SNOMEDCTTrait (Prelude.Maybe SNOMEDCTTraitName)
sNOMEDCTTrait_name = Lens.lens (\SNOMEDCTTrait' {name} -> name) (\s@SNOMEDCTTrait' {} a -> s {name = a} :: SNOMEDCTTrait)

-- | The level of confidence that Comprehend Medical has in the accuracy of a
-- detected trait.
sNOMEDCTTrait_score :: Lens.Lens' SNOMEDCTTrait (Prelude.Maybe Prelude.Double)
sNOMEDCTTrait_score = Lens.lens (\SNOMEDCTTrait' {score} -> score) (\s@SNOMEDCTTrait' {} a -> s {score = a} :: SNOMEDCTTrait)

instance Data.FromJSON SNOMEDCTTrait where
  parseJSON =
    Data.withObject
      "SNOMEDCTTrait"
      ( \x ->
          SNOMEDCTTrait'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable SNOMEDCTTrait where
  hashWithSalt _salt SNOMEDCTTrait' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` score

instance Prelude.NFData SNOMEDCTTrait where
  rnf SNOMEDCTTrait' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf score
