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
-- Module      : Network.AWS.ElastiCache.Types.TagListMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.TagListMessage where

import Network.AWS.ElastiCache.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output from the @AddTagsToResource@,
-- @ListTagsForResource@, and @RemoveTagsFromResource@ operations.
--
-- /See:/ 'newTagListMessage' smart constructor.
data TagListMessage = TagListMessage'
  { -- | A list of cost allocation tags as key-value pairs.
    tagList :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagListMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'tagListMessage_tagList' - A list of cost allocation tags as key-value pairs.
newTagListMessage ::
  TagListMessage
newTagListMessage =
  TagListMessage' {tagList = Prelude.Nothing}

-- | A list of cost allocation tags as key-value pairs.
tagListMessage_tagList :: Lens.Lens' TagListMessage (Prelude.Maybe [Tag])
tagListMessage_tagList = Lens.lens (\TagListMessage' {tagList} -> tagList) (\s@TagListMessage' {} a -> s {tagList = a} :: TagListMessage) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML TagListMessage where
  parseXML x =
    TagListMessage'
      Prelude.<$> ( x Prelude..@? "TagList" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Tag")
                  )

instance Prelude.Hashable TagListMessage

instance Prelude.NFData TagListMessage
