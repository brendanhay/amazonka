{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RecrawlBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlBehavior
  ( RecrawlBehavior
      ( RecrawlBehavior',
        CrawlEverything,
        CrawlNewFoldersOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RecrawlBehavior = RecrawlBehavior' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CrawlEverything :: RecrawlBehavior
pattern CrawlEverything = RecrawlBehavior' "CRAWL_EVERYTHING"

pattern CrawlNewFoldersOnly :: RecrawlBehavior
pattern CrawlNewFoldersOnly = RecrawlBehavior' "CRAWL_NEW_FOLDERS_ONLY"

{-# COMPLETE
  CrawlEverything,
  CrawlNewFoldersOnly,
  RecrawlBehavior'
  #-}
