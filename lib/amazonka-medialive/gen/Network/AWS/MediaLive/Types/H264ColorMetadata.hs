-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ColorMetadata
  ( H264ColorMetadata
      ( H264ColorMetadata',
        HIgnore,
        HInsert
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Color Metadata
newtype H264ColorMetadata = H264ColorMetadata' Lude.Text
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

pattern HIgnore :: H264ColorMetadata
pattern HIgnore = H264ColorMetadata' "IGNORE"

pattern HInsert :: H264ColorMetadata
pattern HInsert = H264ColorMetadata' "INSERT"

{-# COMPLETE
  HIgnore,
  HInsert,
  H264ColorMetadata'
  #-}
