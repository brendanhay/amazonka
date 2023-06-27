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
-- Module      : Amazonka.QuickSight.Types.TopicRangeFilterConstant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRangeFilterConstant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConstantType
import Amazonka.QuickSight.Types.RangeConstant

-- | A constant value that is used in a range filter to specify the endpoints
-- of the range.
--
-- /See:/ 'newTopicRangeFilterConstant' smart constructor.
data TopicRangeFilterConstant = TopicRangeFilterConstant'
  { -- | The data type of the constant value that is used in a range filter.
    -- Valid values for this structure are @RANGE@.
    constantType :: Prelude.Maybe ConstantType,
    -- | The value of the constant that is used to specify the endpoints of a
    -- range filter.
    rangeConstant :: Prelude.Maybe RangeConstant
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRangeFilterConstant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constantType', 'topicRangeFilterConstant_constantType' - The data type of the constant value that is used in a range filter.
-- Valid values for this structure are @RANGE@.
--
-- 'rangeConstant', 'topicRangeFilterConstant_rangeConstant' - The value of the constant that is used to specify the endpoints of a
-- range filter.
newTopicRangeFilterConstant ::
  TopicRangeFilterConstant
newTopicRangeFilterConstant =
  TopicRangeFilterConstant'
    { constantType =
        Prelude.Nothing,
      rangeConstant = Prelude.Nothing
    }

-- | The data type of the constant value that is used in a range filter.
-- Valid values for this structure are @RANGE@.
topicRangeFilterConstant_constantType :: Lens.Lens' TopicRangeFilterConstant (Prelude.Maybe ConstantType)
topicRangeFilterConstant_constantType = Lens.lens (\TopicRangeFilterConstant' {constantType} -> constantType) (\s@TopicRangeFilterConstant' {} a -> s {constantType = a} :: TopicRangeFilterConstant)

-- | The value of the constant that is used to specify the endpoints of a
-- range filter.
topicRangeFilterConstant_rangeConstant :: Lens.Lens' TopicRangeFilterConstant (Prelude.Maybe RangeConstant)
topicRangeFilterConstant_rangeConstant = Lens.lens (\TopicRangeFilterConstant' {rangeConstant} -> rangeConstant) (\s@TopicRangeFilterConstant' {} a -> s {rangeConstant = a} :: TopicRangeFilterConstant)

instance Data.FromJSON TopicRangeFilterConstant where
  parseJSON =
    Data.withObject
      "TopicRangeFilterConstant"
      ( \x ->
          TopicRangeFilterConstant'
            Prelude.<$> (x Data..:? "ConstantType")
            Prelude.<*> (x Data..:? "RangeConstant")
      )

instance Prelude.Hashable TopicRangeFilterConstant where
  hashWithSalt _salt TopicRangeFilterConstant' {..} =
    _salt
      `Prelude.hashWithSalt` constantType
      `Prelude.hashWithSalt` rangeConstant

instance Prelude.NFData TopicRangeFilterConstant where
  rnf TopicRangeFilterConstant' {..} =
    Prelude.rnf constantType
      `Prelude.seq` Prelude.rnf rangeConstant

instance Data.ToJSON TopicRangeFilterConstant where
  toJSON TopicRangeFilterConstant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConstantType" Data..=) Prelude.<$> constantType,
            ("RangeConstant" Data..=) Prelude.<$> rangeConstant
          ]
      )
