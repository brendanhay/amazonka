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
-- Module      : Amazonka.PersonalizeRuntime.Types.PredictedItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeRuntime.Types.PredictedItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that identifies an item.
--
-- The and APIs return a list of @PredictedItem@s.
--
-- /See:/ 'newPredictedItem' smart constructor.
data PredictedItem = PredictedItem'
  { -- | The recommended item ID.
    itemId :: Prelude.Maybe Prelude.Text,
    -- | The name of the promotion that included the predicted item.
    promotionName :: Prelude.Maybe Prelude.Text,
    -- | A numeric representation of the model\'s certainty that the item will be
    -- the next user selection. For more information on scoring logic, see
    -- how-scores-work.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemId', 'predictedItem_itemId' - The recommended item ID.
--
-- 'promotionName', 'predictedItem_promotionName' - The name of the promotion that included the predicted item.
--
-- 'score', 'predictedItem_score' - A numeric representation of the model\'s certainty that the item will be
-- the next user selection. For more information on scoring logic, see
-- how-scores-work.
newPredictedItem ::
  PredictedItem
newPredictedItem =
  PredictedItem'
    { itemId = Prelude.Nothing,
      promotionName = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The recommended item ID.
predictedItem_itemId :: Lens.Lens' PredictedItem (Prelude.Maybe Prelude.Text)
predictedItem_itemId = Lens.lens (\PredictedItem' {itemId} -> itemId) (\s@PredictedItem' {} a -> s {itemId = a} :: PredictedItem)

-- | The name of the promotion that included the predicted item.
predictedItem_promotionName :: Lens.Lens' PredictedItem (Prelude.Maybe Prelude.Text)
predictedItem_promotionName = Lens.lens (\PredictedItem' {promotionName} -> promotionName) (\s@PredictedItem' {} a -> s {promotionName = a} :: PredictedItem)

-- | A numeric representation of the model\'s certainty that the item will be
-- the next user selection. For more information on scoring logic, see
-- how-scores-work.
predictedItem_score :: Lens.Lens' PredictedItem (Prelude.Maybe Prelude.Double)
predictedItem_score = Lens.lens (\PredictedItem' {score} -> score) (\s@PredictedItem' {} a -> s {score = a} :: PredictedItem)

instance Data.FromJSON PredictedItem where
  parseJSON =
    Data.withObject
      "PredictedItem"
      ( \x ->
          PredictedItem'
            Prelude.<$> (x Data..:? "itemId")
            Prelude.<*> (x Data..:? "promotionName")
            Prelude.<*> (x Data..:? "score")
      )

instance Prelude.Hashable PredictedItem where
  hashWithSalt _salt PredictedItem' {..} =
    _salt `Prelude.hashWithSalt` itemId
      `Prelude.hashWithSalt` promotionName
      `Prelude.hashWithSalt` score

instance Prelude.NFData PredictedItem where
  rnf PredictedItem' {..} =
    Prelude.rnf itemId
      `Prelude.seq` Prelude.rnf promotionName
      `Prelude.seq` Prelude.rnf score
