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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    tag :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  TagFilter
newTagFilter pTag_ = TagFilter' {tag = pTag_}

-- | Specifies the tag that must be associated with the execution for it to
-- meet the filter criteria.
--
-- Tags may only contain unicode letters, digits, whitespace, or these
-- symbols: @_ . : \/ = + - \@@.
tagFilter_tag :: Lens.Lens' TagFilter Core.Text
tagFilter_tag = Lens.lens (\TagFilter' {tag} -> tag) (\s@TagFilter' {} a -> s {tag = a} :: TagFilter)

instance Core.Hashable TagFilter

instance Core.NFData TagFilter

instance Core.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("tag" Core..= tag)])
