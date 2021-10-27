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
-- Module      : Network.AWS.LicenseManager.Types.Metadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types.Metadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes key\/value pairs.
--
-- /See:/ 'newMetadata' smart constructor.
data Metadata = Metadata'
  { -- | The value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key name.
    name :: Prelude.Maybe Prelude.Text
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
-- 'value', 'metadata_value' - The value.
--
-- 'name', 'metadata_name' - The key name.
newMetadata ::
  Metadata
newMetadata =
  Metadata'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value.
metadata_value :: Lens.Lens' Metadata (Prelude.Maybe Prelude.Text)
metadata_value = Lens.lens (\Metadata' {value} -> value) (\s@Metadata' {} a -> s {value = a} :: Metadata)

-- | The key name.
metadata_name :: Lens.Lens' Metadata (Prelude.Maybe Prelude.Text)
metadata_name = Lens.lens (\Metadata' {name} -> name) (\s@Metadata' {} a -> s {name = a} :: Metadata)

instance Core.FromJSON Metadata where
  parseJSON =
    Core.withObject
      "Metadata"
      ( \x ->
          Metadata'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable Metadata

instance Prelude.NFData Metadata

instance Core.ToJSON Metadata where
  toJSON Metadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
