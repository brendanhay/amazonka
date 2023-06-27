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
-- Module      : Amazonka.FraudDetector.Types.UncertaintyRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.UncertaintyRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Range of area under curve (auc) expected from the model. A range greater
-- than 0.1 indicates higher model uncertainity. A range is the difference
-- between upper and lower bound of auc.
--
-- /See:/ 'newUncertaintyRange' smart constructor.
data UncertaintyRange = UncertaintyRange'
  { -- | The lower bound value of the area under curve (auc).
    lowerBoundValue :: Prelude.Double,
    -- | The upper bound value of the area under curve (auc).
    upperBoundValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UncertaintyRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBoundValue', 'uncertaintyRange_lowerBoundValue' - The lower bound value of the area under curve (auc).
--
-- 'upperBoundValue', 'uncertaintyRange_upperBoundValue' - The upper bound value of the area under curve (auc).
newUncertaintyRange ::
  -- | 'lowerBoundValue'
  Prelude.Double ->
  -- | 'upperBoundValue'
  Prelude.Double ->
  UncertaintyRange
newUncertaintyRange
  pLowerBoundValue_
  pUpperBoundValue_ =
    UncertaintyRange'
      { lowerBoundValue =
          pLowerBoundValue_,
        upperBoundValue = pUpperBoundValue_
      }

-- | The lower bound value of the area under curve (auc).
uncertaintyRange_lowerBoundValue :: Lens.Lens' UncertaintyRange Prelude.Double
uncertaintyRange_lowerBoundValue = Lens.lens (\UncertaintyRange' {lowerBoundValue} -> lowerBoundValue) (\s@UncertaintyRange' {} a -> s {lowerBoundValue = a} :: UncertaintyRange)

-- | The upper bound value of the area under curve (auc).
uncertaintyRange_upperBoundValue :: Lens.Lens' UncertaintyRange Prelude.Double
uncertaintyRange_upperBoundValue = Lens.lens (\UncertaintyRange' {upperBoundValue} -> upperBoundValue) (\s@UncertaintyRange' {} a -> s {upperBoundValue = a} :: UncertaintyRange)

instance Data.FromJSON UncertaintyRange where
  parseJSON =
    Data.withObject
      "UncertaintyRange"
      ( \x ->
          UncertaintyRange'
            Prelude.<$> (x Data..: "lowerBoundValue")
            Prelude.<*> (x Data..: "upperBoundValue")
      )

instance Prelude.Hashable UncertaintyRange where
  hashWithSalt _salt UncertaintyRange' {..} =
    _salt
      `Prelude.hashWithSalt` lowerBoundValue
      `Prelude.hashWithSalt` upperBoundValue

instance Prelude.NFData UncertaintyRange where
  rnf UncertaintyRange' {..} =
    Prelude.rnf lowerBoundValue
      `Prelude.seq` Prelude.rnf upperBoundValue
