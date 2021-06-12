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
-- Module      : Network.AWS.Inspector.Types.ResourceGroupTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ResourceGroupTag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used as one of the elements of the ResourceGroup data
-- type.
--
-- /See:/ 'newResourceGroupTag' smart constructor.
data ResourceGroupTag = ResourceGroupTag'
  { -- | The value assigned to a tag key.
    value :: Core.Maybe Core.Text,
    -- | A tag key.
    key :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourceGroupTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceGroupTag_value' - The value assigned to a tag key.
--
-- 'key', 'resourceGroupTag_key' - A tag key.
newResourceGroupTag ::
  -- | 'key'
  Core.Text ->
  ResourceGroupTag
newResourceGroupTag pKey_ =
  ResourceGroupTag'
    { value = Core.Nothing,
      key = pKey_
    }

-- | The value assigned to a tag key.
resourceGroupTag_value :: Lens.Lens' ResourceGroupTag (Core.Maybe Core.Text)
resourceGroupTag_value = Lens.lens (\ResourceGroupTag' {value} -> value) (\s@ResourceGroupTag' {} a -> s {value = a} :: ResourceGroupTag)

-- | A tag key.
resourceGroupTag_key :: Lens.Lens' ResourceGroupTag Core.Text
resourceGroupTag_key = Lens.lens (\ResourceGroupTag' {key} -> key) (\s@ResourceGroupTag' {} a -> s {key = a} :: ResourceGroupTag)

instance Core.FromJSON ResourceGroupTag where
  parseJSON =
    Core.withObject
      "ResourceGroupTag"
      ( \x ->
          ResourceGroupTag'
            Core.<$> (x Core..:? "value") Core.<*> (x Core..: "key")
      )

instance Core.Hashable ResourceGroupTag

instance Core.NFData ResourceGroupTag

instance Core.ToJSON ResourceGroupTag where
  toJSON ResourceGroupTag' {..} =
    Core.object
      ( Core.catMaybes
          [ ("value" Core..=) Core.<$> value,
            Core.Just ("key" Core..= key)
          ]
      )
