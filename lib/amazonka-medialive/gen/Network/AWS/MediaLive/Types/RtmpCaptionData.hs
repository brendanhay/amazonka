{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionData
  ( RtmpCaptionData
      ( RtmpCaptionData',
        All,
        FIELD1608,
        FIELD1AndFIELD2608
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Rtmp Caption Data
newtype RtmpCaptionData = RtmpCaptionData' Lude.Text
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

pattern All :: RtmpCaptionData
pattern All = RtmpCaptionData' "ALL"

pattern FIELD1608 :: RtmpCaptionData
pattern FIELD1608 = RtmpCaptionData' "FIELD1_608"

pattern FIELD1AndFIELD2608 :: RtmpCaptionData
pattern FIELD1AndFIELD2608 = RtmpCaptionData' "FIELD1_AND_FIELD2_608"

{-# COMPLETE
  All,
  FIELD1608,
  FIELD1AndFIELD2608,
  RtmpCaptionData'
  #-}
