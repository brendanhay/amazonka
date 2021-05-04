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
-- Module      : Network.AWS.CloudFront.Types.TagKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.TagKeys where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'newTagKeys' smart constructor.
data TagKeys = TagKeys'
  { -- | A complex type that contains @Tag@ key elements.
    items :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newTagKeys = TagKeys' {items = Prelude.Nothing}

-- | A complex type that contains @Tag@ key elements.
tagKeys_items :: Lens.Lens' TagKeys (Prelude.Maybe [Prelude.Text])
tagKeys_items = Lens.lens (\TagKeys' {items} -> items) (\s@TagKeys' {} a -> s {items = a} :: TagKeys) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable TagKeys

instance Prelude.NFData TagKeys

instance Prelude.ToXML TagKeys where
  toXML TagKeys' {..} =
    Prelude.mconcat
      [ "Items"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Key" Prelude.<$> items)
      ]
