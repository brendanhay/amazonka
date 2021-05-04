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
-- Module      : Network.AWS.Glue.Types.ConfusionMatrix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConfusionMatrix where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The confusion matrix shows you what your transform is predicting
-- accurately and what types of errors it is making.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in
-- Wikipedia.
--
-- /See:/ 'newConfusionMatrix' smart constructor.
data ConfusionMatrix = ConfusionMatrix'
  { -- | The number of nonmatches in the data that the transform incorrectly
    -- classified as a match, in the confusion matrix for your transform.
    numFalsePositives :: Prelude.Maybe Prelude.Integer,
    -- | The number of nonmatches in the data that the transform correctly
    -- rejected, in the confusion matrix for your transform.
    numTrueNegatives :: Prelude.Maybe Prelude.Integer,
    -- | The number of matches in the data that the transform didn\'t find, in
    -- the confusion matrix for your transform.
    numFalseNegatives :: Prelude.Maybe Prelude.Integer,
    -- | The number of matches in the data that the transform correctly found, in
    -- the confusion matrix for your transform.
    numTruePositives :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConfusionMatrix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numFalsePositives', 'confusionMatrix_numFalsePositives' - The number of nonmatches in the data that the transform incorrectly
-- classified as a match, in the confusion matrix for your transform.
--
-- 'numTrueNegatives', 'confusionMatrix_numTrueNegatives' - The number of nonmatches in the data that the transform correctly
-- rejected, in the confusion matrix for your transform.
--
-- 'numFalseNegatives', 'confusionMatrix_numFalseNegatives' - The number of matches in the data that the transform didn\'t find, in
-- the confusion matrix for your transform.
--
-- 'numTruePositives', 'confusionMatrix_numTruePositives' - The number of matches in the data that the transform correctly found, in
-- the confusion matrix for your transform.
newConfusionMatrix ::
  ConfusionMatrix
newConfusionMatrix =
  ConfusionMatrix'
    { numFalsePositives =
        Prelude.Nothing,
      numTrueNegatives = Prelude.Nothing,
      numFalseNegatives = Prelude.Nothing,
      numTruePositives = Prelude.Nothing
    }

-- | The number of nonmatches in the data that the transform incorrectly
-- classified as a match, in the confusion matrix for your transform.
confusionMatrix_numFalsePositives :: Lens.Lens' ConfusionMatrix (Prelude.Maybe Prelude.Integer)
confusionMatrix_numFalsePositives = Lens.lens (\ConfusionMatrix' {numFalsePositives} -> numFalsePositives) (\s@ConfusionMatrix' {} a -> s {numFalsePositives = a} :: ConfusionMatrix)

-- | The number of nonmatches in the data that the transform correctly
-- rejected, in the confusion matrix for your transform.
confusionMatrix_numTrueNegatives :: Lens.Lens' ConfusionMatrix (Prelude.Maybe Prelude.Integer)
confusionMatrix_numTrueNegatives = Lens.lens (\ConfusionMatrix' {numTrueNegatives} -> numTrueNegatives) (\s@ConfusionMatrix' {} a -> s {numTrueNegatives = a} :: ConfusionMatrix)

-- | The number of matches in the data that the transform didn\'t find, in
-- the confusion matrix for your transform.
confusionMatrix_numFalseNegatives :: Lens.Lens' ConfusionMatrix (Prelude.Maybe Prelude.Integer)
confusionMatrix_numFalseNegatives = Lens.lens (\ConfusionMatrix' {numFalseNegatives} -> numFalseNegatives) (\s@ConfusionMatrix' {} a -> s {numFalseNegatives = a} :: ConfusionMatrix)

-- | The number of matches in the data that the transform correctly found, in
-- the confusion matrix for your transform.
confusionMatrix_numTruePositives :: Lens.Lens' ConfusionMatrix (Prelude.Maybe Prelude.Integer)
confusionMatrix_numTruePositives = Lens.lens (\ConfusionMatrix' {numTruePositives} -> numTruePositives) (\s@ConfusionMatrix' {} a -> s {numTruePositives = a} :: ConfusionMatrix)

instance Prelude.FromJSON ConfusionMatrix where
  parseJSON =
    Prelude.withObject
      "ConfusionMatrix"
      ( \x ->
          ConfusionMatrix'
            Prelude.<$> (x Prelude..:? "NumFalsePositives")
            Prelude.<*> (x Prelude..:? "NumTrueNegatives")
            Prelude.<*> (x Prelude..:? "NumFalseNegatives")
            Prelude.<*> (x Prelude..:? "NumTruePositives")
      )

instance Prelude.Hashable ConfusionMatrix

instance Prelude.NFData ConfusionMatrix
