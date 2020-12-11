-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacSpec
  ( AacSpec
      ( AacSpec',
        ASMPEG2,
        ASMPEG4
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Aac Spec
newtype AacSpec = AacSpec' Lude.Text
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

pattern ASMPEG2 :: AacSpec
pattern ASMPEG2 = AacSpec' "MPEG2"

pattern ASMPEG4 :: AacSpec
pattern ASMPEG4 = AacSpec' "MPEG4"

{-# COMPLETE
  ASMPEG2,
  ASMPEG4,
  AacSpec'
  #-}
