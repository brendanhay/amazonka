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
-- Module      : Amazonka.QuickSight.Types.NullValueFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NullValueFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options that determine the null value format configuration.
--
-- /See:/ 'newNullValueFormatConfiguration' smart constructor.
data NullValueFormatConfiguration = NullValueFormatConfiguration'
  { -- | Determines the null string of null values.
    nullString :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NullValueFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nullString', 'nullValueFormatConfiguration_nullString' - Determines the null string of null values.
newNullValueFormatConfiguration ::
  -- | 'nullString'
  Prelude.Text ->
  NullValueFormatConfiguration
newNullValueFormatConfiguration pNullString_ =
  NullValueFormatConfiguration'
    { nullString =
        Data._Sensitive Lens.# pNullString_
    }

-- | Determines the null string of null values.
nullValueFormatConfiguration_nullString :: Lens.Lens' NullValueFormatConfiguration Prelude.Text
nullValueFormatConfiguration_nullString = Lens.lens (\NullValueFormatConfiguration' {nullString} -> nullString) (\s@NullValueFormatConfiguration' {} a -> s {nullString = a} :: NullValueFormatConfiguration) Prelude.. Data._Sensitive

instance Data.FromJSON NullValueFormatConfiguration where
  parseJSON =
    Data.withObject
      "NullValueFormatConfiguration"
      ( \x ->
          NullValueFormatConfiguration'
            Prelude.<$> (x Data..: "NullString")
      )

instance
  Prelude.Hashable
    NullValueFormatConfiguration
  where
  hashWithSalt _salt NullValueFormatConfiguration' {..} =
    _salt `Prelude.hashWithSalt` nullString

instance Prelude.NFData NullValueFormatConfiguration where
  rnf NullValueFormatConfiguration' {..} =
    Prelude.rnf nullString

instance Data.ToJSON NullValueFormatConfiguration where
  toJSON NullValueFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NullString" Data..= nullString)]
      )
