-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.StreamViewType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.StreamViewType
  ( StreamViewType
      ( StreamViewType',
        KeysOnly,
        NewAndOldImages,
        NewImage,
        OldImage
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StreamViewType = StreamViewType' Lude.Text
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

pattern KeysOnly :: StreamViewType
pattern KeysOnly = StreamViewType' "KEYS_ONLY"

pattern NewAndOldImages :: StreamViewType
pattern NewAndOldImages = StreamViewType' "NEW_AND_OLD_IMAGES"

pattern NewImage :: StreamViewType
pattern NewImage = StreamViewType' "NEW_IMAGE"

pattern OldImage :: StreamViewType
pattern OldImage = StreamViewType' "OLD_IMAGE"

{-# COMPLETE
  KeysOnly,
  NewAndOldImages,
  NewImage,
  OldImage,
  StreamViewType'
  #-}
