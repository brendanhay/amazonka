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
-- Module      : Amazonka.Glue.Types.AmazonRedshiftAdvancedOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AmazonRedshiftAdvancedOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an optional value when connecting to the Redshift cluster.
--
-- /See:/ 'newAmazonRedshiftAdvancedOption' smart constructor.
data AmazonRedshiftAdvancedOption = AmazonRedshiftAdvancedOption'
  { -- | The key for the additional connection option.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value for the additional connection option.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonRedshiftAdvancedOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'amazonRedshiftAdvancedOption_key' - The key for the additional connection option.
--
-- 'value', 'amazonRedshiftAdvancedOption_value' - The value for the additional connection option.
newAmazonRedshiftAdvancedOption ::
  AmazonRedshiftAdvancedOption
newAmazonRedshiftAdvancedOption =
  AmazonRedshiftAdvancedOption'
    { key =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key for the additional connection option.
amazonRedshiftAdvancedOption_key :: Lens.Lens' AmazonRedshiftAdvancedOption (Prelude.Maybe Prelude.Text)
amazonRedshiftAdvancedOption_key = Lens.lens (\AmazonRedshiftAdvancedOption' {key} -> key) (\s@AmazonRedshiftAdvancedOption' {} a -> s {key = a} :: AmazonRedshiftAdvancedOption)

-- | The value for the additional connection option.
amazonRedshiftAdvancedOption_value :: Lens.Lens' AmazonRedshiftAdvancedOption (Prelude.Maybe Prelude.Text)
amazonRedshiftAdvancedOption_value = Lens.lens (\AmazonRedshiftAdvancedOption' {value} -> value) (\s@AmazonRedshiftAdvancedOption' {} a -> s {value = a} :: AmazonRedshiftAdvancedOption)

instance Data.FromJSON AmazonRedshiftAdvancedOption where
  parseJSON =
    Data.withObject
      "AmazonRedshiftAdvancedOption"
      ( \x ->
          AmazonRedshiftAdvancedOption'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    AmazonRedshiftAdvancedOption
  where
  hashWithSalt _salt AmazonRedshiftAdvancedOption' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData AmazonRedshiftAdvancedOption where
  rnf AmazonRedshiftAdvancedOption' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AmazonRedshiftAdvancedOption where
  toJSON AmazonRedshiftAdvancedOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
