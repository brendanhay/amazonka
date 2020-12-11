-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264GopBReference
  ( H264GopBReference
      ( H264GopBReference',
        HGBRGDisabled,
        HGBRGEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H264GopBReference = H264GopBReference' Lude.Text
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

pattern HGBRGDisabled :: H264GopBReference
pattern HGBRGDisabled = H264GopBReference' "DISABLED"

pattern HGBRGEnabled :: H264GopBReference
pattern HGBRGEnabled = H264GopBReference' "ENABLED"

{-# COMPLETE
  HGBRGDisabled,
  HGBRGEnabled,
  H264GopBReference'
  #-}
