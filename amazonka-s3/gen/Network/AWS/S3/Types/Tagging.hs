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
-- Module      : Network.AWS.S3.Types.Tagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Tagging where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | Container for @TagSet@ elements.
--
-- /See:/ 'newTagging' smart constructor.
data Tagging = Tagging'
  { -- | A collection for a set of tags
    tagSet :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Tagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSet', 'tagging_tagSet' - A collection for a set of tags
newTagging ::
  Tagging
newTagging = Tagging' {tagSet = Prelude.mempty}

-- | A collection for a set of tags
tagging_tagSet :: Lens.Lens' Tagging [Tag]
tagging_tagSet = Lens.lens (\Tagging' {tagSet} -> tagSet) (\s@Tagging' {} a -> s {tagSet = a} :: Tagging) Prelude.. Prelude._Coerce

instance Prelude.Hashable Tagging

instance Prelude.NFData Tagging

instance Prelude.ToXML Tagging where
  toXML Tagging' {..} =
    Prelude.mconcat
      ["TagSet" Prelude.@= Prelude.toXMLList "Tag" tagSet]
