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
-- Module      : Amazonka.QuickSight.Types.DataBarsOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataBarsOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options for data bars.
--
-- /See:/ 'newDataBarsOptions' smart constructor.
data DataBarsOptions = DataBarsOptions'
  { -- | The color of the negative data bar.
    negativeColor :: Prelude.Maybe Prelude.Text,
    -- | The color of the positive data bar.
    positiveColor :: Prelude.Maybe Prelude.Text,
    -- | The field ID for the data bars options.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataBarsOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negativeColor', 'dataBarsOptions_negativeColor' - The color of the negative data bar.
--
-- 'positiveColor', 'dataBarsOptions_positiveColor' - The color of the positive data bar.
--
-- 'fieldId', 'dataBarsOptions_fieldId' - The field ID for the data bars options.
newDataBarsOptions ::
  -- | 'fieldId'
  Prelude.Text ->
  DataBarsOptions
newDataBarsOptions pFieldId_ =
  DataBarsOptions'
    { negativeColor = Prelude.Nothing,
      positiveColor = Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The color of the negative data bar.
dataBarsOptions_negativeColor :: Lens.Lens' DataBarsOptions (Prelude.Maybe Prelude.Text)
dataBarsOptions_negativeColor = Lens.lens (\DataBarsOptions' {negativeColor} -> negativeColor) (\s@DataBarsOptions' {} a -> s {negativeColor = a} :: DataBarsOptions)

-- | The color of the positive data bar.
dataBarsOptions_positiveColor :: Lens.Lens' DataBarsOptions (Prelude.Maybe Prelude.Text)
dataBarsOptions_positiveColor = Lens.lens (\DataBarsOptions' {positiveColor} -> positiveColor) (\s@DataBarsOptions' {} a -> s {positiveColor = a} :: DataBarsOptions)

-- | The field ID for the data bars options.
dataBarsOptions_fieldId :: Lens.Lens' DataBarsOptions Prelude.Text
dataBarsOptions_fieldId = Lens.lens (\DataBarsOptions' {fieldId} -> fieldId) (\s@DataBarsOptions' {} a -> s {fieldId = a} :: DataBarsOptions)

instance Data.FromJSON DataBarsOptions where
  parseJSON =
    Data.withObject
      "DataBarsOptions"
      ( \x ->
          DataBarsOptions'
            Prelude.<$> (x Data..:? "NegativeColor")
            Prelude.<*> (x Data..:? "PositiveColor")
            Prelude.<*> (x Data..: "FieldId")
      )

instance Prelude.Hashable DataBarsOptions where
  hashWithSalt _salt DataBarsOptions' {..} =
    _salt
      `Prelude.hashWithSalt` negativeColor
      `Prelude.hashWithSalt` positiveColor
      `Prelude.hashWithSalt` fieldId

instance Prelude.NFData DataBarsOptions where
  rnf DataBarsOptions' {..} =
    Prelude.rnf negativeColor
      `Prelude.seq` Prelude.rnf positiveColor
      `Prelude.seq` Prelude.rnf fieldId

instance Data.ToJSON DataBarsOptions where
  toJSON DataBarsOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NegativeColor" Data..=) Prelude.<$> negativeColor,
            ("PositiveColor" Data..=) Prelude.<$> positiveColor,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
