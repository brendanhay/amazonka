{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SVTNewImage,
        SVTOldImage,
        SVTNewAndOldImages,
        SVTKeysOnly
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

pattern SVTNewImage :: StreamViewType
pattern SVTNewImage = StreamViewType' "NEW_IMAGE"

pattern SVTOldImage :: StreamViewType
pattern SVTOldImage = StreamViewType' "OLD_IMAGE"

pattern SVTNewAndOldImages :: StreamViewType
pattern SVTNewAndOldImages = StreamViewType' "NEW_AND_OLD_IMAGES"

pattern SVTKeysOnly :: StreamViewType
pattern SVTKeysOnly = StreamViewType' "KEYS_ONLY"

{-# COMPLETE
  SVTNewImage,
  SVTOldImage,
  SVTNewAndOldImages,
  SVTKeysOnly,
  StreamViewType'
  #-}
