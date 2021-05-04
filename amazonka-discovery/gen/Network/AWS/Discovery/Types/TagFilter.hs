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
-- Module      : Network.AWS.Discovery.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.TagFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The tag filter. Valid names are: @tagKey@, @tagValue@,
-- @configurationId@.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | A name of the tag filter.
    name :: Prelude.Text,
    -- | Values for the tag filter.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'tagFilter_name' - A name of the tag filter.
--
-- 'values', 'tagFilter_values' - Values for the tag filter.
newTagFilter ::
  -- | 'name'
  Prelude.Text ->
  TagFilter
newTagFilter pName_ =
  TagFilter' {name = pName_, values = Prelude.mempty}

-- | A name of the tag filter.
tagFilter_name :: Lens.Lens' TagFilter Prelude.Text
tagFilter_name = Lens.lens (\TagFilter' {name} -> name) (\s@TagFilter' {} a -> s {name = a} :: TagFilter)

-- | Values for the tag filter.
tagFilter_values :: Lens.Lens' TagFilter [Prelude.Text]
tagFilter_values = Lens.lens (\TagFilter' {values} -> values) (\s@TagFilter' {} a -> s {values = a} :: TagFilter) Prelude.. Prelude._Coerce

instance Prelude.Hashable TagFilter

instance Prelude.NFData TagFilter

instance Prelude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("values" Prelude..= values)
          ]
      )
