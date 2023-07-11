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
-- Module      : Amazonka.QuickSight.Types.NumberFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumberFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NumericFormatConfiguration

-- | Formatting configuration for number fields.
--
-- /See:/ 'newNumberFormatConfiguration' smart constructor.
data NumberFormatConfiguration = NumberFormatConfiguration'
  { -- | The options that determine the numeric format configuration.
    formatConfiguration :: Prelude.Maybe NumericFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfiguration', 'numberFormatConfiguration_formatConfiguration' - The options that determine the numeric format configuration.
newNumberFormatConfiguration ::
  NumberFormatConfiguration
newNumberFormatConfiguration =
  NumberFormatConfiguration'
    { formatConfiguration =
        Prelude.Nothing
    }

-- | The options that determine the numeric format configuration.
numberFormatConfiguration_formatConfiguration :: Lens.Lens' NumberFormatConfiguration (Prelude.Maybe NumericFormatConfiguration)
numberFormatConfiguration_formatConfiguration = Lens.lens (\NumberFormatConfiguration' {formatConfiguration} -> formatConfiguration) (\s@NumberFormatConfiguration' {} a -> s {formatConfiguration = a} :: NumberFormatConfiguration)

instance Data.FromJSON NumberFormatConfiguration where
  parseJSON =
    Data.withObject
      "NumberFormatConfiguration"
      ( \x ->
          NumberFormatConfiguration'
            Prelude.<$> (x Data..:? "FormatConfiguration")
      )

instance Prelude.Hashable NumberFormatConfiguration where
  hashWithSalt _salt NumberFormatConfiguration' {..} =
    _salt `Prelude.hashWithSalt` formatConfiguration

instance Prelude.NFData NumberFormatConfiguration where
  rnf NumberFormatConfiguration' {..} =
    Prelude.rnf formatConfiguration

instance Data.ToJSON NumberFormatConfiguration where
  toJSON NumberFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration
          ]
      )
