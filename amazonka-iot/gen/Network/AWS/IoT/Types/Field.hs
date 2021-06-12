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
-- Module      : Network.AWS.IoT.Types.Field
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Field where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.FieldType
import qualified Network.AWS.Lens as Lens

-- | Describes the name and data type at a field.
--
-- /See:/ 'newField' smart constructor.
data Field = Field'
  { -- | The name of the field.
    name :: Core.Maybe Core.Text,
    -- | The datatype of the field.
    type' :: Core.Maybe FieldType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Field' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'field_name' - The name of the field.
--
-- 'type'', 'field_type' - The datatype of the field.
newField ::
  Field
newField =
  Field' {name = Core.Nothing, type' = Core.Nothing}

-- | The name of the field.
field_name :: Lens.Lens' Field (Core.Maybe Core.Text)
field_name = Lens.lens (\Field' {name} -> name) (\s@Field' {} a -> s {name = a} :: Field)

-- | The datatype of the field.
field_type :: Lens.Lens' Field (Core.Maybe FieldType)
field_type = Lens.lens (\Field' {type'} -> type') (\s@Field' {} a -> s {type' = a} :: Field)

instance Core.FromJSON Field where
  parseJSON =
    Core.withObject
      "Field"
      ( \x ->
          Field'
            Core.<$> (x Core..:? "name") Core.<*> (x Core..:? "type")
      )

instance Core.Hashable Field

instance Core.NFData Field

instance Core.ToJSON Field where
  toJSON Field' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("type" Core..=) Core.<$> type'
          ]
      )
