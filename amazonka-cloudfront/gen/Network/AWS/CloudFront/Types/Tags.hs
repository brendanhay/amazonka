{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'newTags' smart constructor.
data Tags = Tags'
  { -- | A complex type that contains @Tag@ elements.
    items :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newTags = Tags' {items = Prelude.Nothing}

-- | A complex type that contains @Tag@ elements.
tags_items :: Lens.Lens' Tags (Prelude.Maybe [Tag])
tags_items = Lens.lens (\Tags' {items} -> items) (\s@Tags' {} a -> s {items = a} :: Tags) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML Tags where
  parseXML x =
    Tags'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )

instance Prelude.Hashable Tags

instance Prelude.NFData Tags

instance Prelude.ToXML Tags where
  toXML Tags' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Tag" Prelude.<$> items)
      ]
