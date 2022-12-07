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
-- Module      : Amazonka.SageMaker.Types.SourceAlgorithmSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SourceAlgorithmSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.SourceAlgorithm

-- | A list of algorithms that were used to create a model package.
--
-- /See:/ 'newSourceAlgorithmSpecification' smart constructor.
data SourceAlgorithmSpecification = SourceAlgorithmSpecification'
  { -- | A list of the algorithms that were used to create a model package.
    sourceAlgorithms :: Prelude.NonEmpty SourceAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceAlgorithmSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAlgorithms', 'sourceAlgorithmSpecification_sourceAlgorithms' - A list of the algorithms that were used to create a model package.
newSourceAlgorithmSpecification ::
  -- | 'sourceAlgorithms'
  Prelude.NonEmpty SourceAlgorithm ->
  SourceAlgorithmSpecification
newSourceAlgorithmSpecification pSourceAlgorithms_ =
  SourceAlgorithmSpecification'
    { sourceAlgorithms =
        Lens.coerced Lens.# pSourceAlgorithms_
    }

-- | A list of the algorithms that were used to create a model package.
sourceAlgorithmSpecification_sourceAlgorithms :: Lens.Lens' SourceAlgorithmSpecification (Prelude.NonEmpty SourceAlgorithm)
sourceAlgorithmSpecification_sourceAlgorithms = Lens.lens (\SourceAlgorithmSpecification' {sourceAlgorithms} -> sourceAlgorithms) (\s@SourceAlgorithmSpecification' {} a -> s {sourceAlgorithms = a} :: SourceAlgorithmSpecification) Prelude.. Lens.coerced

instance Data.FromJSON SourceAlgorithmSpecification where
  parseJSON =
    Data.withObject
      "SourceAlgorithmSpecification"
      ( \x ->
          SourceAlgorithmSpecification'
            Prelude.<$> (x Data..: "SourceAlgorithms")
      )

instance
  Prelude.Hashable
    SourceAlgorithmSpecification
  where
  hashWithSalt _salt SourceAlgorithmSpecification' {..} =
    _salt `Prelude.hashWithSalt` sourceAlgorithms

instance Prelude.NFData SourceAlgorithmSpecification where
  rnf SourceAlgorithmSpecification' {..} =
    Prelude.rnf sourceAlgorithms

instance Data.ToJSON SourceAlgorithmSpecification where
  toJSON SourceAlgorithmSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SourceAlgorithms" Data..= sourceAlgorithms)
          ]
      )
