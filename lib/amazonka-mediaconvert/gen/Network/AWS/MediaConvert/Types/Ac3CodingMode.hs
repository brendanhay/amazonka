-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3CodingMode
  ( Ac3CodingMode
      ( Ac3CodingMode',
        CodingMode10,
        CodingMode11,
        CodingMode20,
        CodingMode32Lfe
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dolby Digital coding mode. Determines number of channels.
newtype Ac3CodingMode = Ac3CodingMode' Lude.Text
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

pattern CodingMode10 :: Ac3CodingMode
pattern CodingMode10 = Ac3CodingMode' "CODING_MODE_1_0"

pattern CodingMode11 :: Ac3CodingMode
pattern CodingMode11 = Ac3CodingMode' "CODING_MODE_1_1"

pattern CodingMode20 :: Ac3CodingMode
pattern CodingMode20 = Ac3CodingMode' "CODING_MODE_2_0"

pattern CodingMode32Lfe :: Ac3CodingMode
pattern CodingMode32Lfe = Ac3CodingMode' "CODING_MODE_3_2_LFE"

{-# COMPLETE
  CodingMode10,
  CodingMode11,
  CodingMode20,
  CodingMode32Lfe,
  Ac3CodingMode'
  #-}
