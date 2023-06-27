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
-- Module      : Amazonka.QuickSight.Types.CollectiveConstant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CollectiveConstant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents a collective constant.
--
-- /See:/ 'newCollectiveConstant' smart constructor.
data CollectiveConstant = CollectiveConstant'
  { -- | A list of values for the collective constant.
    valueList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectiveConstant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valueList', 'collectiveConstant_valueList' - A list of values for the collective constant.
newCollectiveConstant ::
  CollectiveConstant
newCollectiveConstant =
  CollectiveConstant' {valueList = Prelude.Nothing}

-- | A list of values for the collective constant.
collectiveConstant_valueList :: Lens.Lens' CollectiveConstant (Prelude.Maybe [Prelude.Text])
collectiveConstant_valueList = Lens.lens (\CollectiveConstant' {valueList} -> valueList) (\s@CollectiveConstant' {} a -> s {valueList = a} :: CollectiveConstant) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CollectiveConstant where
  parseJSON =
    Data.withObject
      "CollectiveConstant"
      ( \x ->
          CollectiveConstant'
            Prelude.<$> (x Data..:? "ValueList" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CollectiveConstant where
  hashWithSalt _salt CollectiveConstant' {..} =
    _salt `Prelude.hashWithSalt` valueList

instance Prelude.NFData CollectiveConstant where
  rnf CollectiveConstant' {..} = Prelude.rnf valueList

instance Data.ToJSON CollectiveConstant where
  toJSON CollectiveConstant' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ValueList" Data..=) Prelude.<$> valueList]
      )
