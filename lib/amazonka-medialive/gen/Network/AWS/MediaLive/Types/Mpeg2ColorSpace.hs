-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ColorSpace
  ( Mpeg2ColorSpace
      ( Mpeg2ColorSpace',
        MCSAuto,
        MCSPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Mpeg2 Color Space
newtype Mpeg2ColorSpace = Mpeg2ColorSpace' Lude.Text
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

pattern MCSAuto :: Mpeg2ColorSpace
pattern MCSAuto = Mpeg2ColorSpace' "AUTO"

pattern MCSPassthrough :: Mpeg2ColorSpace
pattern MCSPassthrough = Mpeg2ColorSpace' "PASSTHROUGH"

{-# COMPLETE
  MCSAuto,
  MCSPassthrough,
  Mpeg2ColorSpace'
  #-}
