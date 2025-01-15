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
-- Module      : Amazonka.Glue.Types.TransformParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TransformParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.FindMatchesParameters
import Amazonka.Glue.Types.TransformType
import qualified Amazonka.Prelude as Prelude

-- | The algorithm-specific parameters that are associated with the machine
-- learning transform.
--
-- /See:/ 'newTransformParameters' smart constructor.
data TransformParameters = TransformParameters'
  { -- | The parameters for the find matches algorithm.
    findMatchesParameters :: Prelude.Maybe FindMatchesParameters,
    -- | The type of machine learning transform.
    --
    -- For information about the types of machine learning transforms, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms>.
    transformType :: TransformType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findMatchesParameters', 'transformParameters_findMatchesParameters' - The parameters for the find matches algorithm.
--
-- 'transformType', 'transformParameters_transformType' - The type of machine learning transform.
--
-- For information about the types of machine learning transforms, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms>.
newTransformParameters ::
  -- | 'transformType'
  TransformType ->
  TransformParameters
newTransformParameters pTransformType_ =
  TransformParameters'
    { findMatchesParameters =
        Prelude.Nothing,
      transformType = pTransformType_
    }

-- | The parameters for the find matches algorithm.
transformParameters_findMatchesParameters :: Lens.Lens' TransformParameters (Prelude.Maybe FindMatchesParameters)
transformParameters_findMatchesParameters = Lens.lens (\TransformParameters' {findMatchesParameters} -> findMatchesParameters) (\s@TransformParameters' {} a -> s {findMatchesParameters = a} :: TransformParameters)

-- | The type of machine learning transform.
--
-- For information about the types of machine learning transforms, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-job-machine-learning-transform.html Creating Machine Learning Transforms>.
transformParameters_transformType :: Lens.Lens' TransformParameters TransformType
transformParameters_transformType = Lens.lens (\TransformParameters' {transformType} -> transformType) (\s@TransformParameters' {} a -> s {transformType = a} :: TransformParameters)

instance Data.FromJSON TransformParameters where
  parseJSON =
    Data.withObject
      "TransformParameters"
      ( \x ->
          TransformParameters'
            Prelude.<$> (x Data..:? "FindMatchesParameters")
            Prelude.<*> (x Data..: "TransformType")
      )

instance Prelude.Hashable TransformParameters where
  hashWithSalt _salt TransformParameters' {..} =
    _salt
      `Prelude.hashWithSalt` findMatchesParameters
      `Prelude.hashWithSalt` transformType

instance Prelude.NFData TransformParameters where
  rnf TransformParameters' {..} =
    Prelude.rnf findMatchesParameters `Prelude.seq`
      Prelude.rnf transformType

instance Data.ToJSON TransformParameters where
  toJSON TransformParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FindMatchesParameters" Data..=)
              Prelude.<$> findMatchesParameters,
            Prelude.Just
              ("TransformType" Data..= transformType)
          ]
      )
