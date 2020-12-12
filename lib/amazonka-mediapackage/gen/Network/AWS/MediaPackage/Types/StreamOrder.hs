{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.StreamOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.StreamOrder
  ( StreamOrder
      ( StreamOrder',
        Original,
        VideoBitrateAscending,
        VideoBitrateDescending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StreamOrder = StreamOrder' Lude.Text
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

pattern Original :: StreamOrder
pattern Original = StreamOrder' "ORIGINAL"

pattern VideoBitrateAscending :: StreamOrder
pattern VideoBitrateAscending = StreamOrder' "VIDEO_BITRATE_ASCENDING"

pattern VideoBitrateDescending :: StreamOrder
pattern VideoBitrateDescending = StreamOrder' "VIDEO_BITRATE_DESCENDING"

{-# COMPLETE
  Original,
  VideoBitrateAscending,
  VideoBitrateDescending,
  StreamOrder'
  #-}
