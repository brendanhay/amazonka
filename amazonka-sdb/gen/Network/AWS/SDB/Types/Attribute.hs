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
-- Module      : Network.AWS.SDB.Types.Attribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Attribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- |
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { alternateNameEncoding :: Core.Maybe Core.Text,
    alternateValueEncoding :: Core.Maybe Core.Text,
    -- | The name of the attribute.
    name :: Core.Text,
    -- | The value of the attribute.
    value :: Core.Text
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
-- 'alternateNameEncoding', 'attribute_alternateNameEncoding' -
--
-- 'alternateValueEncoding', 'attribute_alternateValueEncoding' -
--
-- 'name', 'attribute_name' - The name of the attribute.
--
-- 'value', 'attribute_value' - The value of the attribute.
newAttribute ::
  -- | 'name'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  Attribute
newAttribute pName_ pValue_ =
  Attribute'
    { alternateNameEncoding = Core.Nothing,
      alternateValueEncoding = Core.Nothing,
      name = pName_,
      value = pValue_
    }

-- |
attribute_alternateNameEncoding :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_alternateNameEncoding = Lens.lens (\Attribute' {alternateNameEncoding} -> alternateNameEncoding) (\s@Attribute' {} a -> s {alternateNameEncoding = a} :: Attribute)

-- |
attribute_alternateValueEncoding :: Lens.Lens' Attribute (Core.Maybe Core.Text)
attribute_alternateValueEncoding = Lens.lens (\Attribute' {alternateValueEncoding} -> alternateValueEncoding) (\s@Attribute' {} a -> s {alternateValueEncoding = a} :: Attribute)

-- | The name of the attribute.
attribute_name :: Lens.Lens' Attribute Core.Text
attribute_name = Lens.lens (\Attribute' {name} -> name) (\s@Attribute' {} a -> s {name = a} :: Attribute)

-- | The value of the attribute.
attribute_value :: Lens.Lens' Attribute Core.Text
attribute_value = Lens.lens (\Attribute' {value} -> value) (\s@Attribute' {} a -> s {value = a} :: Attribute)

instance Core.FromXML Attribute where
  parseXML x =
    Attribute'
      Core.<$> (x Core..@? "AlternateNameEncoding")
      Core.<*> (x Core..@? "AlternateValueEncoding")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "Value")

instance Core.Hashable Attribute

instance Core.NFData Attribute

instance Core.ToQuery Attribute where
  toQuery Attribute' {..} =
    Core.mconcat
      [ "AlternateNameEncoding"
          Core.=: alternateNameEncoding,
        "AlternateValueEncoding"
          Core.=: alternateValueEncoding,
        "Name" Core.=: name,
        "Value" Core.=: value
      ]
