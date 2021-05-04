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
-- Module      : Network.AWS.SWF.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.TagFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to filter the workflow executions in visibility APIs based on a
-- tag.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | Specifies the tag that must be associated with the execution for it to
    -- meet the filter criteria.
    --
    -- Tags may only contain unicode letters, digits, whitespace, or these
    -- symbols: @_ . : \/ = + - \@@.
    tag :: Prelude.Text
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
-- 'tag', 'tagFilter_tag' - Specifies the tag that must be associated with the execution for it to
-- meet the filter criteria.
--
-- Tags may only contain unicode letters, digits, whitespace, or these
-- symbols: @_ . : \/ = + - \@@.
newTagFilter ::
  -- | 'tag'
  Prelude.Text ->
  TagFilter
newTagFilter pTag_ = TagFilter' {tag = pTag_}

-- | Specifies the tag that must be associated with the execution for it to
-- meet the filter criteria.
--
-- Tags may only contain unicode letters, digits, whitespace, or these
-- symbols: @_ . : \/ = + - \@@.
tagFilter_tag :: Lens.Lens' TagFilter Prelude.Text
tagFilter_tag = Lens.lens (\TagFilter' {tag} -> tag) (\s@TagFilter' {} a -> s {tag = a} :: TagFilter)

instance Prelude.Hashable TagFilter

instance Prelude.NFData TagFilter

instance Prelude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("tag" Prelude..= tag)]
      )
