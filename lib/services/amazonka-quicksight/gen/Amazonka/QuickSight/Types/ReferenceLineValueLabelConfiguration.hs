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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineValueLabelConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineValueLabelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NumericFormatConfiguration
import Amazonka.QuickSight.Types.ReferenceLineValueLabelRelativePosition

-- | The value label configuration of the label in a reference line.
--
-- /See:/ 'newReferenceLineValueLabelConfiguration' smart constructor.
data ReferenceLineValueLabelConfiguration = ReferenceLineValueLabelConfiguration'
  { -- | The format configuration of the value label.
    formatConfiguration :: Prelude.Maybe NumericFormatConfiguration,
    -- | The relative position of the value label. Choose one of the following
    -- options:
    --
    -- -   @BEFORE_CUSTOM_LABEL@
    --
    -- -   @AFTER_CUSTOM_LABEL@
    relativePosition :: Prelude.Maybe ReferenceLineValueLabelRelativePosition
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineValueLabelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfiguration', 'referenceLineValueLabelConfiguration_formatConfiguration' - The format configuration of the value label.
--
-- 'relativePosition', 'referenceLineValueLabelConfiguration_relativePosition' - The relative position of the value label. Choose one of the following
-- options:
--
-- -   @BEFORE_CUSTOM_LABEL@
--
-- -   @AFTER_CUSTOM_LABEL@
newReferenceLineValueLabelConfiguration ::
  ReferenceLineValueLabelConfiguration
newReferenceLineValueLabelConfiguration =
  ReferenceLineValueLabelConfiguration'
    { formatConfiguration =
        Prelude.Nothing,
      relativePosition = Prelude.Nothing
    }

-- | The format configuration of the value label.
referenceLineValueLabelConfiguration_formatConfiguration :: Lens.Lens' ReferenceLineValueLabelConfiguration (Prelude.Maybe NumericFormatConfiguration)
referenceLineValueLabelConfiguration_formatConfiguration = Lens.lens (\ReferenceLineValueLabelConfiguration' {formatConfiguration} -> formatConfiguration) (\s@ReferenceLineValueLabelConfiguration' {} a -> s {formatConfiguration = a} :: ReferenceLineValueLabelConfiguration)

-- | The relative position of the value label. Choose one of the following
-- options:
--
-- -   @BEFORE_CUSTOM_LABEL@
--
-- -   @AFTER_CUSTOM_LABEL@
referenceLineValueLabelConfiguration_relativePosition :: Lens.Lens' ReferenceLineValueLabelConfiguration (Prelude.Maybe ReferenceLineValueLabelRelativePosition)
referenceLineValueLabelConfiguration_relativePosition = Lens.lens (\ReferenceLineValueLabelConfiguration' {relativePosition} -> relativePosition) (\s@ReferenceLineValueLabelConfiguration' {} a -> s {relativePosition = a} :: ReferenceLineValueLabelConfiguration)

instance
  Data.FromJSON
    ReferenceLineValueLabelConfiguration
  where
  parseJSON =
    Data.withObject
      "ReferenceLineValueLabelConfiguration"
      ( \x ->
          ReferenceLineValueLabelConfiguration'
            Prelude.<$> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..:? "RelativePosition")
      )

instance
  Prelude.Hashable
    ReferenceLineValueLabelConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineValueLabelConfiguration' {..} =
      _salt `Prelude.hashWithSalt` formatConfiguration
        `Prelude.hashWithSalt` relativePosition

instance
  Prelude.NFData
    ReferenceLineValueLabelConfiguration
  where
  rnf ReferenceLineValueLabelConfiguration' {..} =
    Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf relativePosition

instance
  Data.ToJSON
    ReferenceLineValueLabelConfiguration
  where
  toJSON ReferenceLineValueLabelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            ("RelativePosition" Data..=)
              Prelude.<$> relativePosition
          ]
      )
