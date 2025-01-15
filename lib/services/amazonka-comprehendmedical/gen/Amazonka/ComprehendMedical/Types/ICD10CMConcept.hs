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
-- Module      : Amazonka.ComprehendMedical.Types.ICD10CMConcept
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ICD10CMConcept where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ICD-10-CM concepts that the entity could refer to, along with a
-- score indicating the likelihood of the match.
--
-- /See:/ 'newICD10CMConcept' smart constructor.
data ICD10CMConcept = ICD10CMConcept'
  { -- | The ICD-10-CM code that identifies the concept found in the knowledge
    -- base from the Centers for Disease Control.
    code :: Prelude.Maybe Prelude.Text,
    -- | The long description of the ICD-10-CM code in the ontology.
    description :: Prelude.Maybe Prelude.Text,
    -- | The level of confidence that Amazon Comprehend Medical has that the
    -- entity is accurately linked to an ICD-10-CM concept.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ICD10CMConcept' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'iCD10CMConcept_code' - The ICD-10-CM code that identifies the concept found in the knowledge
-- base from the Centers for Disease Control.
--
-- 'description', 'iCD10CMConcept_description' - The long description of the ICD-10-CM code in the ontology.
--
-- 'score', 'iCD10CMConcept_score' - The level of confidence that Amazon Comprehend Medical has that the
-- entity is accurately linked to an ICD-10-CM concept.
newICD10CMConcept ::
  ICD10CMConcept
newICD10CMConcept =
  ICD10CMConcept'
    { code = Prelude.Nothing,
      description = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The ICD-10-CM code that identifies the concept found in the knowledge
-- base from the Centers for Disease Control.
iCD10CMConcept_code :: Lens.Lens' ICD10CMConcept (Prelude.Maybe Prelude.Text)
iCD10CMConcept_code = Lens.lens (\ICD10CMConcept' {code} -> code) (\s@ICD10CMConcept' {} a -> s {code = a} :: ICD10CMConcept)

-- | The long description of the ICD-10-CM code in the ontology.
iCD10CMConcept_description :: Lens.Lens' ICD10CMConcept (Prelude.Maybe Prelude.Text)
iCD10CMConcept_description = Lens.lens (\ICD10CMConcept' {description} -> description) (\s@ICD10CMConcept' {} a -> s {description = a} :: ICD10CMConcept)

-- | The level of confidence that Amazon Comprehend Medical has that the
-- entity is accurately linked to an ICD-10-CM concept.
iCD10CMConcept_score :: Lens.Lens' ICD10CMConcept (Prelude.Maybe Prelude.Double)
iCD10CMConcept_score = Lens.lens (\ICD10CMConcept' {score} -> score) (\s@ICD10CMConcept' {} a -> s {score = a} :: ICD10CMConcept)

instance Data.FromJSON ICD10CMConcept where
  parseJSON =
    Data.withObject
      "ICD10CMConcept"
      ( \x ->
          ICD10CMConcept'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Score")
      )

instance Prelude.Hashable ICD10CMConcept where
  hashWithSalt _salt ICD10CMConcept' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` score

instance Prelude.NFData ICD10CMConcept where
  rnf ICD10CMConcept' {..} =
    Prelude.rnf code `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf score
