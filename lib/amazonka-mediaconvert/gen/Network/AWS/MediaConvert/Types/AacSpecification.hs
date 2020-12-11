-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacSpecification
  ( AacSpecification
      ( AacSpecification',
        ASMPEG2,
        ASMPEG4
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
newtype AacSpecification = AacSpecification' Lude.Text
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

pattern ASMPEG2 :: AacSpecification
pattern ASMPEG2 = AacSpecification' "MPEG2"

pattern ASMPEG4 :: AacSpecification
pattern ASMPEG4 = AacSpecification' "MPEG4"

{-# COMPLETE
  ASMPEG2,
  ASMPEG4,
  AacSpecification'
  #-}
