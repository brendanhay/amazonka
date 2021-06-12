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
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilter where

import Network.AWS.CodeDeploy.Types.EC2TagFilterType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an EC2 tag filter.
--
-- /See:/ 'newEC2TagFilter' smart constructor.
data EC2TagFilter = EC2TagFilter'
  { -- | The tag filter key.
    key :: Core.Maybe Core.Text,
    -- | The tag filter value.
    value :: Core.Maybe Core.Text,
    -- | The tag filter type:
    --
    -- -   @KEY_ONLY@: Key only.
    --
    -- -   @VALUE_ONLY@: Value only.
    --
    -- -   @KEY_AND_VALUE@: Key and value.
    type' :: Core.Maybe EC2TagFilterType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EC2TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'eC2TagFilter_key' - The tag filter key.
--
-- 'value', 'eC2TagFilter_value' - The tag filter value.
--
-- 'type'', 'eC2TagFilter_type' - The tag filter type:
--
-- -   @KEY_ONLY@: Key only.
--
-- -   @VALUE_ONLY@: Value only.
--
-- -   @KEY_AND_VALUE@: Key and value.
newEC2TagFilter ::
  EC2TagFilter
newEC2TagFilter =
  EC2TagFilter'
    { key = Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing
    }

-- | The tag filter key.
eC2TagFilter_key :: Lens.Lens' EC2TagFilter (Core.Maybe Core.Text)
eC2TagFilter_key = Lens.lens (\EC2TagFilter' {key} -> key) (\s@EC2TagFilter' {} a -> s {key = a} :: EC2TagFilter)

-- | The tag filter value.
eC2TagFilter_value :: Lens.Lens' EC2TagFilter (Core.Maybe Core.Text)
eC2TagFilter_value = Lens.lens (\EC2TagFilter' {value} -> value) (\s@EC2TagFilter' {} a -> s {value = a} :: EC2TagFilter)

-- | The tag filter type:
--
-- -   @KEY_ONLY@: Key only.
--
-- -   @VALUE_ONLY@: Value only.
--
-- -   @KEY_AND_VALUE@: Key and value.
eC2TagFilter_type :: Lens.Lens' EC2TagFilter (Core.Maybe EC2TagFilterType)
eC2TagFilter_type = Lens.lens (\EC2TagFilter' {type'} -> type') (\s@EC2TagFilter' {} a -> s {type' = a} :: EC2TagFilter)

instance Core.FromJSON EC2TagFilter where
  parseJSON =
    Core.withObject
      "EC2TagFilter"
      ( \x ->
          EC2TagFilter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable EC2TagFilter

instance Core.NFData EC2TagFilter

instance Core.ToJSON EC2TagFilter where
  toJSON EC2TagFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value,
            ("Type" Core..=) Core.<$> type'
          ]
      )
