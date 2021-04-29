{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.Types.TransformParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TransformParameters where

import Network.AWS.Glue.Types.FindMatchesParameters
import Network.AWS.Glue.Types.TransformType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON TransformParameters where
  parseJSON =
    Prelude.withObject
      "TransformParameters"
      ( \x ->
          TransformParameters'
            Prelude.<$> (x Prelude..:? "FindMatchesParameters")
            Prelude.<*> (x Prelude..: "TransformType")
      )

instance Prelude.Hashable TransformParameters

instance Prelude.NFData TransformParameters

instance Prelude.ToJSON TransformParameters where
  toJSON TransformParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FindMatchesParameters" Prelude..=)
              Prelude.<$> findMatchesParameters,
            Prelude.Just
              ("TransformType" Prelude..= transformType)
          ]
      )
