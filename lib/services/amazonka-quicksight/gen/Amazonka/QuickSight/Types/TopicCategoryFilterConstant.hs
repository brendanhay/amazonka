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
-- Module      : Amazonka.QuickSight.Types.TopicCategoryFilterConstant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicCategoryFilterConstant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CollectiveConstant
import Amazonka.QuickSight.Types.ConstantType

-- | A constant used in a category filter.
--
-- /See:/ 'newTopicCategoryFilterConstant' smart constructor.
data TopicCategoryFilterConstant = TopicCategoryFilterConstant'
  { -- | A collective constant used in a category filter. This element is used to
    -- specify a list of values for the constant.
    collectiveConstant :: Prelude.Maybe CollectiveConstant,
    -- | The type of category filter constant. This element is used to specify
    -- whether a constant is a singular or collective. Valid values are
    -- @SINGULAR@ and @COLLECTIVE@.
    constantType :: Prelude.Maybe ConstantType,
    -- | A singular constant used in a category filter. This element is used to
    -- specify a single value for the constant.
    singularConstant :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicCategoryFilterConstant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectiveConstant', 'topicCategoryFilterConstant_collectiveConstant' - A collective constant used in a category filter. This element is used to
-- specify a list of values for the constant.
--
-- 'constantType', 'topicCategoryFilterConstant_constantType' - The type of category filter constant. This element is used to specify
-- whether a constant is a singular or collective. Valid values are
-- @SINGULAR@ and @COLLECTIVE@.
--
-- 'singularConstant', 'topicCategoryFilterConstant_singularConstant' - A singular constant used in a category filter. This element is used to
-- specify a single value for the constant.
newTopicCategoryFilterConstant ::
  TopicCategoryFilterConstant
newTopicCategoryFilterConstant =
  TopicCategoryFilterConstant'
    { collectiveConstant =
        Prelude.Nothing,
      constantType = Prelude.Nothing,
      singularConstant = Prelude.Nothing
    }

-- | A collective constant used in a category filter. This element is used to
-- specify a list of values for the constant.
topicCategoryFilterConstant_collectiveConstant :: Lens.Lens' TopicCategoryFilterConstant (Prelude.Maybe CollectiveConstant)
topicCategoryFilterConstant_collectiveConstant = Lens.lens (\TopicCategoryFilterConstant' {collectiveConstant} -> collectiveConstant) (\s@TopicCategoryFilterConstant' {} a -> s {collectiveConstant = a} :: TopicCategoryFilterConstant)

-- | The type of category filter constant. This element is used to specify
-- whether a constant is a singular or collective. Valid values are
-- @SINGULAR@ and @COLLECTIVE@.
topicCategoryFilterConstant_constantType :: Lens.Lens' TopicCategoryFilterConstant (Prelude.Maybe ConstantType)
topicCategoryFilterConstant_constantType = Lens.lens (\TopicCategoryFilterConstant' {constantType} -> constantType) (\s@TopicCategoryFilterConstant' {} a -> s {constantType = a} :: TopicCategoryFilterConstant)

-- | A singular constant used in a category filter. This element is used to
-- specify a single value for the constant.
topicCategoryFilterConstant_singularConstant :: Lens.Lens' TopicCategoryFilterConstant (Prelude.Maybe Prelude.Text)
topicCategoryFilterConstant_singularConstant = Lens.lens (\TopicCategoryFilterConstant' {singularConstant} -> singularConstant) (\s@TopicCategoryFilterConstant' {} a -> s {singularConstant = a} :: TopicCategoryFilterConstant)

instance Data.FromJSON TopicCategoryFilterConstant where
  parseJSON =
    Data.withObject
      "TopicCategoryFilterConstant"
      ( \x ->
          TopicCategoryFilterConstant'
            Prelude.<$> (x Data..:? "CollectiveConstant")
            Prelude.<*> (x Data..:? "ConstantType")
            Prelude.<*> (x Data..:? "SingularConstant")
      )

instance Prelude.Hashable TopicCategoryFilterConstant where
  hashWithSalt _salt TopicCategoryFilterConstant' {..} =
    _salt
      `Prelude.hashWithSalt` collectiveConstant
      `Prelude.hashWithSalt` constantType
      `Prelude.hashWithSalt` singularConstant

instance Prelude.NFData TopicCategoryFilterConstant where
  rnf TopicCategoryFilterConstant' {..} =
    Prelude.rnf collectiveConstant
      `Prelude.seq` Prelude.rnf constantType
      `Prelude.seq` Prelude.rnf singularConstant

instance Data.ToJSON TopicCategoryFilterConstant where
  toJSON TopicCategoryFilterConstant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CollectiveConstant" Data..=)
              Prelude.<$> collectiveConstant,
            ("ConstantType" Data..=) Prelude.<$> constantType,
            ("SingularConstant" Data..=)
              Prelude.<$> singularConstant
          ]
      )
