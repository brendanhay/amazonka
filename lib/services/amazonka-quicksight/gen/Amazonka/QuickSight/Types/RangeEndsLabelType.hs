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
-- Module      : Amazonka.QuickSight.Types.RangeEndsLabelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RangeEndsLabelType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The range ends label type of a data path label.
--
-- /See:/ 'newRangeEndsLabelType' smart constructor.
data RangeEndsLabelType = RangeEndsLabelType'
  { -- | The visibility of the range ends label.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RangeEndsLabelType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'rangeEndsLabelType_visibility' - The visibility of the range ends label.
newRangeEndsLabelType ::
  RangeEndsLabelType
newRangeEndsLabelType =
  RangeEndsLabelType' {visibility = Prelude.Nothing}

-- | The visibility of the range ends label.
rangeEndsLabelType_visibility :: Lens.Lens' RangeEndsLabelType (Prelude.Maybe Visibility)
rangeEndsLabelType_visibility = Lens.lens (\RangeEndsLabelType' {visibility} -> visibility) (\s@RangeEndsLabelType' {} a -> s {visibility = a} :: RangeEndsLabelType)

instance Data.FromJSON RangeEndsLabelType where
  parseJSON =
    Data.withObject
      "RangeEndsLabelType"
      ( \x ->
          RangeEndsLabelType'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable RangeEndsLabelType where
  hashWithSalt _salt RangeEndsLabelType' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData RangeEndsLabelType where
  rnf RangeEndsLabelType' {..} = Prelude.rnf visibility

instance Data.ToJSON RangeEndsLabelType where
  toJSON RangeEndsLabelType' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
