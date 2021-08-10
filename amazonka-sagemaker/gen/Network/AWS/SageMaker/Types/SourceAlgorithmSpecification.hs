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
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithmSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.SourceAlgorithm

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
        Lens._Coerce Lens.# pSourceAlgorithms_
    }

-- | A list of the algorithms that were used to create a model package.
sourceAlgorithmSpecification_sourceAlgorithms :: Lens.Lens' SourceAlgorithmSpecification (Prelude.NonEmpty SourceAlgorithm)
sourceAlgorithmSpecification_sourceAlgorithms = Lens.lens (\SourceAlgorithmSpecification' {sourceAlgorithms} -> sourceAlgorithms) (\s@SourceAlgorithmSpecification' {} a -> s {sourceAlgorithms = a} :: SourceAlgorithmSpecification) Prelude.. Lens._Coerce

instance Core.FromJSON SourceAlgorithmSpecification where
  parseJSON =
    Core.withObject
      "SourceAlgorithmSpecification"
      ( \x ->
          SourceAlgorithmSpecification'
            Prelude.<$> (x Core..: "SourceAlgorithms")
      )

instance
  Prelude.Hashable
    SourceAlgorithmSpecification

instance Prelude.NFData SourceAlgorithmSpecification

instance Core.ToJSON SourceAlgorithmSpecification where
  toJSON SourceAlgorithmSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SourceAlgorithms" Core..= sourceAlgorithms)
          ]
      )
