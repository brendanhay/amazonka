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
-- Module      : Network.AWS.CloudFront.Types.Tags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tags where

import Network.AWS.CloudFront.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'newTags' smart constructor.
data Tags = Tags'
  { -- | A complex type that contains @Tag@ elements.
    items :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'tags_items' - A complex type that contains @Tag@ elements.
newTags ::
  Tags
newTags = Tags' {items = Core.Nothing}

-- | A complex type that contains @Tag@ elements.
tags_items :: Lens.Lens' Tags (Core.Maybe [Tag])
tags_items = Lens.lens (\Tags' {items} -> items) (\s@Tags' {} a -> s {items = a} :: Tags) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML Tags where
  parseXML x =
    Tags'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )

instance Core.Hashable Tags

instance Core.NFData Tags

instance Core.ToXML Tags where
  toXML Tags' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML (Core.toXMLList "Tag" Core.<$> items)
      ]
