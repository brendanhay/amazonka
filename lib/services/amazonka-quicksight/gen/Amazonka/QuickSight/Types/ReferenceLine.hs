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
-- Module      : Amazonka.QuickSight.Types.ReferenceLine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLine where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ReferenceLineDataConfiguration
import Amazonka.QuickSight.Types.ReferenceLineLabelConfiguration
import Amazonka.QuickSight.Types.ReferenceLineStyleConfiguration
import Amazonka.QuickSight.Types.WidgetStatus

-- | The reference line visual display options.
--
-- /See:/ 'newReferenceLine' smart constructor.
data ReferenceLine = ReferenceLine'
  { -- | The label configuration of the reference line.
    labelConfiguration :: Prelude.Maybe ReferenceLineLabelConfiguration,
    -- | The status of the reference line. Choose one of the following options:
    --
    -- -   @ENABLE@
    --
    -- -   @DISABLE@
    status :: Prelude.Maybe WidgetStatus,
    -- | The style configuration of the reference line.
    styleConfiguration :: Prelude.Maybe ReferenceLineStyleConfiguration,
    -- | The data configuration of the reference line.
    dataConfiguration :: ReferenceLineDataConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelConfiguration', 'referenceLine_labelConfiguration' - The label configuration of the reference line.
--
-- 'status', 'referenceLine_status' - The status of the reference line. Choose one of the following options:
--
-- -   @ENABLE@
--
-- -   @DISABLE@
--
-- 'styleConfiguration', 'referenceLine_styleConfiguration' - The style configuration of the reference line.
--
-- 'dataConfiguration', 'referenceLine_dataConfiguration' - The data configuration of the reference line.
newReferenceLine ::
  -- | 'dataConfiguration'
  ReferenceLineDataConfiguration ->
  ReferenceLine
newReferenceLine pDataConfiguration_ =
  ReferenceLine'
    { labelConfiguration =
        Prelude.Nothing,
      status = Prelude.Nothing,
      styleConfiguration = Prelude.Nothing,
      dataConfiguration = pDataConfiguration_
    }

-- | The label configuration of the reference line.
referenceLine_labelConfiguration :: Lens.Lens' ReferenceLine (Prelude.Maybe ReferenceLineLabelConfiguration)
referenceLine_labelConfiguration = Lens.lens (\ReferenceLine' {labelConfiguration} -> labelConfiguration) (\s@ReferenceLine' {} a -> s {labelConfiguration = a} :: ReferenceLine)

-- | The status of the reference line. Choose one of the following options:
--
-- -   @ENABLE@
--
-- -   @DISABLE@
referenceLine_status :: Lens.Lens' ReferenceLine (Prelude.Maybe WidgetStatus)
referenceLine_status = Lens.lens (\ReferenceLine' {status} -> status) (\s@ReferenceLine' {} a -> s {status = a} :: ReferenceLine)

-- | The style configuration of the reference line.
referenceLine_styleConfiguration :: Lens.Lens' ReferenceLine (Prelude.Maybe ReferenceLineStyleConfiguration)
referenceLine_styleConfiguration = Lens.lens (\ReferenceLine' {styleConfiguration} -> styleConfiguration) (\s@ReferenceLine' {} a -> s {styleConfiguration = a} :: ReferenceLine)

-- | The data configuration of the reference line.
referenceLine_dataConfiguration :: Lens.Lens' ReferenceLine ReferenceLineDataConfiguration
referenceLine_dataConfiguration = Lens.lens (\ReferenceLine' {dataConfiguration} -> dataConfiguration) (\s@ReferenceLine' {} a -> s {dataConfiguration = a} :: ReferenceLine)

instance Data.FromJSON ReferenceLine where
  parseJSON =
    Data.withObject
      "ReferenceLine"
      ( \x ->
          ReferenceLine'
            Prelude.<$> (x Data..:? "LabelConfiguration")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StyleConfiguration")
            Prelude.<*> (x Data..: "DataConfiguration")
      )

instance Prelude.Hashable ReferenceLine where
  hashWithSalt _salt ReferenceLine' {..} =
    _salt `Prelude.hashWithSalt` labelConfiguration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` styleConfiguration
      `Prelude.hashWithSalt` dataConfiguration

instance Prelude.NFData ReferenceLine where
  rnf ReferenceLine' {..} =
    Prelude.rnf labelConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf styleConfiguration
      `Prelude.seq` Prelude.rnf dataConfiguration

instance Data.ToJSON ReferenceLine where
  toJSON ReferenceLine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelConfiguration" Data..=)
              Prelude.<$> labelConfiguration,
            ("Status" Data..=) Prelude.<$> status,
            ("StyleConfiguration" Data..=)
              Prelude.<$> styleConfiguration,
            Prelude.Just
              ("DataConfiguration" Data..= dataConfiguration)
          ]
      )
