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
-- Module      : Network.AWS.DirectoryService.Types.Attribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Attribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a named directory attribute.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The name of the attribute.
    name :: Core.Maybe Core.Text,
    -- | The value of the attribute.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'attribute_name' - The name of the attribute.
--
-- 'value', 'attribute_value' - The value of the attribute.
newAttribute ::
  Attribute
newAttribute =
  Attribute'
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of the attribute.
attribute_name :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_name = Lens.lens (\Attribute' {name} -> name) (\s@Attribute' {} a -> s {name = a} :: Attribute)

-- | The value of the attribute.
attribute_value :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_value = Lens.lens (\Attribute' {value} -> value) (\s@Attribute' {} a -> s {value = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable Attribute

instance Core.NFData Attribute

instance Core.ToJSON Attribute where
  toJSON Attribute' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Value" Core..=) Core.<$> value
          ]
      )
