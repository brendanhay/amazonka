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
-- Module      : Amazonka.QuickSight.Types.MinimumLabelType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MinimumLabelType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The minimum label of a data path label.
--
-- /See:/ 'newMinimumLabelType' smart constructor.
data MinimumLabelType = MinimumLabelType'
  { -- | The visibility of the minimum label.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MinimumLabelType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'minimumLabelType_visibility' - The visibility of the minimum label.
newMinimumLabelType ::
  MinimumLabelType
newMinimumLabelType =
  MinimumLabelType' {visibility = Prelude.Nothing}

-- | The visibility of the minimum label.
minimumLabelType_visibility :: Lens.Lens' MinimumLabelType (Prelude.Maybe Visibility)
minimumLabelType_visibility = Lens.lens (\MinimumLabelType' {visibility} -> visibility) (\s@MinimumLabelType' {} a -> s {visibility = a} :: MinimumLabelType)

instance Data.FromJSON MinimumLabelType where
  parseJSON =
    Data.withObject
      "MinimumLabelType"
      ( \x ->
          MinimumLabelType'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable MinimumLabelType where
  hashWithSalt _salt MinimumLabelType' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData MinimumLabelType where
  rnf MinimumLabelType' {..} = Prelude.rnf visibility

instance Data.ToJSON MinimumLabelType where
  toJSON MinimumLabelType' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
