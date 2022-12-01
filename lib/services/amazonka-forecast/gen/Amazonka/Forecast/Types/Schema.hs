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
-- Module      : Amazonka.Forecast.Types.Schema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Schema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.SchemaAttribute
import qualified Amazonka.Prelude as Prelude

-- | Defines the fields of a dataset.
--
-- /See:/ 'newSchema' smart constructor.
data Schema = Schema'
  { -- | An array of attributes specifying the name and type of each field in a
    -- dataset.
    attributes :: Prelude.Maybe (Prelude.NonEmpty SchemaAttribute)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'schema_attributes' - An array of attributes specifying the name and type of each field in a
-- dataset.
newSchema ::
  Schema
newSchema = Schema' {attributes = Prelude.Nothing}

-- | An array of attributes specifying the name and type of each field in a
-- dataset.
schema_attributes :: Lens.Lens' Schema (Prelude.Maybe (Prelude.NonEmpty SchemaAttribute))
schema_attributes = Lens.lens (\Schema' {attributes} -> attributes) (\s@Schema' {} a -> s {attributes = a} :: Schema) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Schema where
  parseJSON =
    Core.withObject
      "Schema"
      ( \x ->
          Schema' Prelude.<$> (x Core..:? "Attributes")
      )

instance Prelude.Hashable Schema where
  hashWithSalt _salt Schema' {..} =
    _salt `Prelude.hashWithSalt` attributes

instance Prelude.NFData Schema where
  rnf Schema' {..} = Prelude.rnf attributes

instance Core.ToJSON Schema where
  toJSON Schema' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Attributes" Core..=) Prelude.<$> attributes]
      )
