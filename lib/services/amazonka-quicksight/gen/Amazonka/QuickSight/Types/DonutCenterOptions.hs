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
-- Module      : Amazonka.QuickSight.Types.DonutCenterOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DonutCenterOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The label options of the label that is displayed in the center of a
-- donut chart. This option isn\'t available for pie charts.
--
-- /See:/ 'newDonutCenterOptions' smart constructor.
data DonutCenterOptions = DonutCenterOptions'
  { -- | Determines the visibility of the label in a donut chart. In the Amazon
    -- QuickSight console, this option is called @\'Show total\'@.
    labelVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DonutCenterOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelVisibility', 'donutCenterOptions_labelVisibility' - Determines the visibility of the label in a donut chart. In the Amazon
-- QuickSight console, this option is called @\'Show total\'@.
newDonutCenterOptions ::
  DonutCenterOptions
newDonutCenterOptions =
  DonutCenterOptions'
    { labelVisibility =
        Prelude.Nothing
    }

-- | Determines the visibility of the label in a donut chart. In the Amazon
-- QuickSight console, this option is called @\'Show total\'@.
donutCenterOptions_labelVisibility :: Lens.Lens' DonutCenterOptions (Prelude.Maybe Visibility)
donutCenterOptions_labelVisibility = Lens.lens (\DonutCenterOptions' {labelVisibility} -> labelVisibility) (\s@DonutCenterOptions' {} a -> s {labelVisibility = a} :: DonutCenterOptions)

instance Data.FromJSON DonutCenterOptions where
  parseJSON =
    Data.withObject
      "DonutCenterOptions"
      ( \x ->
          DonutCenterOptions'
            Prelude.<$> (x Data..:? "LabelVisibility")
      )

instance Prelude.Hashable DonutCenterOptions where
  hashWithSalt _salt DonutCenterOptions' {..} =
    _salt `Prelude.hashWithSalt` labelVisibility

instance Prelude.NFData DonutCenterOptions where
  rnf DonutCenterOptions' {..} =
    Prelude.rnf labelVisibility

instance Data.ToJSON DonutCenterOptions where
  toJSON DonutCenterOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelVisibility" Data..=)
              Prelude.<$> labelVisibility
          ]
      )
