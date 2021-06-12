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
-- Module      : Network.AWS.CloudFront.Types.TagKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TagKeys where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'newTagKeys' smart constructor.
data TagKeys = TagKeys'
  { -- | A complex type that contains @Tag@ key elements.
    items :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'tagKeys_items' - A complex type that contains @Tag@ key elements.
newTagKeys ::
  TagKeys
newTagKeys = TagKeys' {items = Core.Nothing}

-- | A complex type that contains @Tag@ key elements.
tagKeys_items :: Lens.Lens' TagKeys (Core.Maybe [Core.Text])
tagKeys_items = Lens.lens (\TagKeys' {items} -> items) (\s@TagKeys' {} a -> s {items = a} :: TagKeys) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable TagKeys

instance Core.NFData TagKeys

instance Core.ToXML TagKeys where
  toXML TagKeys' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML (Core.toXMLList "Key" Core.<$> items)
      ]
