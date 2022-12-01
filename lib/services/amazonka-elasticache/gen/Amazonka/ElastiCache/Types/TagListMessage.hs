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
-- Module      : Amazonka.ElastiCache.Types.TagListMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.TagListMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Represents the output from the @AddTagsToResource@,
-- @ListTagsForResource@, and @RemoveTagsFromResource@ operations.
--
-- /See:/ 'newTagListMessage' smart constructor.
data TagListMessage = TagListMessage'
  { -- | A list of tags as key-value pairs.
    tagList :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagListMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'tagListMessage_tagList' - A list of tags as key-value pairs.
newTagListMessage ::
  TagListMessage
newTagListMessage =
  TagListMessage' {tagList = Prelude.Nothing}

-- | A list of tags as key-value pairs.
tagListMessage_tagList :: Lens.Lens' TagListMessage (Prelude.Maybe [Tag])
tagListMessage_tagList = Lens.lens (\TagListMessage' {tagList} -> tagList) (\s@TagListMessage' {} a -> s {tagList = a} :: TagListMessage) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML TagListMessage where
  parseXML x =
    TagListMessage'
      Prelude.<$> ( x Core..@? "TagList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )

instance Prelude.Hashable TagListMessage where
  hashWithSalt _salt TagListMessage' {..} =
    _salt `Prelude.hashWithSalt` tagList

instance Prelude.NFData TagListMessage where
  rnf TagListMessage' {..} = Prelude.rnf tagList
