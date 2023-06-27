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
-- Module      : Amazonka.QuickSight.Types.TopicSingularFilterConstant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicSingularFilterConstant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConstantType

-- | A structure that represents a singular filter constant, used in filters
-- to specify a single value to match against.
--
-- /See:/ 'newTopicSingularFilterConstant' smart constructor.
data TopicSingularFilterConstant = TopicSingularFilterConstant'
  { -- | The type of the singular filter constant. Valid values for this
    -- structure are @SINGULAR@.
    constantType :: Prelude.Maybe ConstantType,
    -- | The value of the singular filter constant.
    singularConstant :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicSingularFilterConstant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constantType', 'topicSingularFilterConstant_constantType' - The type of the singular filter constant. Valid values for this
-- structure are @SINGULAR@.
--
-- 'singularConstant', 'topicSingularFilterConstant_singularConstant' - The value of the singular filter constant.
newTopicSingularFilterConstant ::
  TopicSingularFilterConstant
newTopicSingularFilterConstant =
  TopicSingularFilterConstant'
    { constantType =
        Prelude.Nothing,
      singularConstant = Prelude.Nothing
    }

-- | The type of the singular filter constant. Valid values for this
-- structure are @SINGULAR@.
topicSingularFilterConstant_constantType :: Lens.Lens' TopicSingularFilterConstant (Prelude.Maybe ConstantType)
topicSingularFilterConstant_constantType = Lens.lens (\TopicSingularFilterConstant' {constantType} -> constantType) (\s@TopicSingularFilterConstant' {} a -> s {constantType = a} :: TopicSingularFilterConstant)

-- | The value of the singular filter constant.
topicSingularFilterConstant_singularConstant :: Lens.Lens' TopicSingularFilterConstant (Prelude.Maybe Prelude.Text)
topicSingularFilterConstant_singularConstant = Lens.lens (\TopicSingularFilterConstant' {singularConstant} -> singularConstant) (\s@TopicSingularFilterConstant' {} a -> s {singularConstant = a} :: TopicSingularFilterConstant)

instance Data.FromJSON TopicSingularFilterConstant where
  parseJSON =
    Data.withObject
      "TopicSingularFilterConstant"
      ( \x ->
          TopicSingularFilterConstant'
            Prelude.<$> (x Data..:? "ConstantType")
            Prelude.<*> (x Data..:? "SingularConstant")
      )

instance Prelude.Hashable TopicSingularFilterConstant where
  hashWithSalt _salt TopicSingularFilterConstant' {..} =
    _salt
      `Prelude.hashWithSalt` constantType
      `Prelude.hashWithSalt` singularConstant

instance Prelude.NFData TopicSingularFilterConstant where
  rnf TopicSingularFilterConstant' {..} =
    Prelude.rnf constantType
      `Prelude.seq` Prelude.rnf singularConstant

instance Data.ToJSON TopicSingularFilterConstant where
  toJSON TopicSingularFilterConstant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConstantType" Data..=) Prelude.<$> constantType,
            ("SingularConstant" Data..=)
              Prelude.<$> singularConstant
          ]
      )
