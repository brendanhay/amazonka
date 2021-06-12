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
-- Module      : Network.AWS.ECR.Types.Attribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Attribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used in the ImageScanFinding data type.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The value assigned to the attribute key.
    value :: Core.Maybe Core.Text,
    -- | The attribute key.
    key :: Core.Text
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
-- 'value', 'attribute_value' - The value assigned to the attribute key.
--
-- 'key', 'attribute_key' - The attribute key.
newAttribute ::
  -- | 'key'
  Core.Text ->
  Attribute
newAttribute pKey_ =
  Attribute' {value = Core.Nothing, key = pKey_}

-- | The value assigned to the attribute key.
attribute_value :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_value = Lens.lens (\Attribute' {value} -> value) (\s@Attribute' {} a -> s {value = a} :: Attribute)

-- | The attribute key.
attribute_key :: Lens.Lens' Attribute Core.Text
attribute_key = Lens.lens (\Attribute' {key} -> key) (\s@Attribute' {} a -> s {key = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Core.<$> (x Core..:? "value") Core.<*> (x Core..: "key")
      )

instance Core.Hashable Attribute

instance Core.NFData Attribute
