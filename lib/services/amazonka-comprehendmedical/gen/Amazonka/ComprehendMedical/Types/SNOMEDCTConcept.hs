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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTConcept
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTConcept where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The SNOMED-CT concepts that the entity could refer to, along with a
-- score indicating the likelihood of the match.
--
-- /See:/ 'newSNOMEDCTConcept' smart constructor.
data SNOMEDCTConcept = SNOMEDCTConcept'
  { -- | The numeric ID for the SNOMED-CT concept.
    code :: Prelude.Maybe Prelude.Text,
    -- | The description of the SNOMED-CT concept.
    description :: Prelude.Maybe Prelude.Text,
    -- | The level of confidence Comprehend Medical has that the entity should be
    -- linked to the identified SNOMED-CT concept.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SNOMEDCTConcept' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'sNOMEDCTConcept_code' - The numeric ID for the SNOMED-CT concept.
--
-- 'description', 'sNOMEDCTConcept_description' - The description of the SNOMED-CT concept.
--
-- 'score', 'sNOMEDCTConcept_score' - The level of confidence Comprehend Medical has that the entity should be
-- linked to the identified SNOMED-CT concept.
newSNOMEDCTConcept ::
  SNOMEDCTConcept
newSNOMEDCTConcept =
  SNOMEDCTConcept'
    { code = Prelude.Nothing,
      description = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The numeric ID for the SNOMED-CT concept.
sNOMEDCTConcept_code :: Lens.Lens' SNOMEDCTConcept (Prelude.Maybe Prelude.Text)
sNOMEDCTConcept_code = Lens.lens (\SNOMEDCTConcept' {code} -> code) (\s@SNOMEDCTConcept' {} a -> s {code = a} :: SNOMEDCTConcept)

-- | The description of the SNOMED-CT concept.
sNOMEDCTConcept_description :: Lens.Lens' SNOMEDCTConcept (Prelude.Maybe Prelude.Text)
sNOMEDCTConcept_description = Lens.lens (\SNOMEDCTConcept' {description} -> description) (\s@SNOMEDCTConcept' {} a -> s {description = a} :: SNOMEDCTConcept)

-- | The level of confidence Comprehend Medical has that the entity should be
-- linked to the identified SNOMED-CT concept.
sNOMEDCTConcept_score :: Lens.Lens' SNOMEDCTConcept (Prelude.Maybe Prelude.Double)
sNOMEDCTConcept_score = Lens.lens (\SNOMEDCTConcept' {score} -> score) (\s@SNOMEDCTConcept' {} a -> s {score = a} :: SNOMEDCTConcept)

instance Data.FromJSON SNOMEDCTConcept where
  parseJSON =
    Data.withObject
      "SNOMEDCTConcept"
      ( \x ->
          SNOMEDCTConcept'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable SNOMEDCTConcept where
  hashWithSalt _salt SNOMEDCTConcept' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` score

instance Prelude.NFData SNOMEDCTConcept where
  rnf SNOMEDCTConcept' {..} =
    Prelude.rnf code `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf score
