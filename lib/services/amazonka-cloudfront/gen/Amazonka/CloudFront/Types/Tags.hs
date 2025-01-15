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
-- Module      : Amazonka.CloudFront.Types.Tags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Tags where

import Amazonka.CloudFront.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /See:/ 'newTags' smart constructor.
data Tags = Tags'
  { -- | A complex type that contains @Tag@ elements.
    items :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
tags_items = Lens.lens (\Tags' {items} -> items) (\s@Tags' {} a -> s {items = a} :: Tags) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML Tags where
  parseXML x =
    Tags'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )

instance Prelude.Hashable Tags where
  hashWithSalt _salt Tags' {..} =
    _salt `Prelude.hashWithSalt` items

instance Prelude.NFData Tags where
  rnf Tags' {..} = Prelude.rnf items

instance Data.ToXML Tags where
  toXML Tags' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML (Data.toXMLList "Tag" Prelude.<$> items)
      ]
