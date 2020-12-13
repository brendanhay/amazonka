{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentThumbnailType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentThumbnailType
  ( DocumentThumbnailType
      ( DocumentThumbnailType',
        Small,
        SmallHq,
        Large
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentThumbnailType = DocumentThumbnailType' Lude.Text
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

pattern Small :: DocumentThumbnailType
pattern Small = DocumentThumbnailType' "SMALL"

pattern SmallHq :: DocumentThumbnailType
pattern SmallHq = DocumentThumbnailType' "SMALL_HQ"

pattern Large :: DocumentThumbnailType
pattern Large = DocumentThumbnailType' "LARGE"

{-# COMPLETE
  Small,
  SmallHq,
  Large,
  DocumentThumbnailType'
  #-}
