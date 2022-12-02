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
-- Module      : Amazonka.LicenseManager.Types.Metadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.Metadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes key\/value pairs.
--
-- /See:/ 'newMetadata' smart constructor.
data Metadata = Metadata'
  { -- | The key name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metadata_name' - The key name.
--
-- 'value', 'metadata_value' - The value.
newMetadata ::
  Metadata
newMetadata =
  Metadata'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key name.
metadata_name :: Lens.Lens' Metadata (Prelude.Maybe Prelude.Text)
metadata_name = Lens.lens (\Metadata' {name} -> name) (\s@Metadata' {} a -> s {name = a} :: Metadata)

-- | The value.
metadata_value :: Lens.Lens' Metadata (Prelude.Maybe Prelude.Text)
metadata_value = Lens.lens (\Metadata' {value} -> value) (\s@Metadata' {} a -> s {value = a} :: Metadata)

instance Data.FromJSON Metadata where
  parseJSON =
    Data.withObject
      "Metadata"
      ( \x ->
          Metadata'
            Prelude.<$> (x Data..:? "Name") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Metadata where
  hashWithSalt _salt Metadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Metadata where
  rnf Metadata' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Metadata where
  toJSON Metadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
