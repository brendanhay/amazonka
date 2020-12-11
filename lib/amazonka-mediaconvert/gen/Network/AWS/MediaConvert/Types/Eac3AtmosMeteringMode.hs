-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
  ( Eac3AtmosMeteringMode
      ( Eac3AtmosMeteringMode',
        EAMMItuBs17701,
        EAMMItuBs17702,
        EAMMItuBs17703,
        EAMMItuBs17704,
        EAMMLeqA
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose how the service meters the loudness of your audio.
newtype Eac3AtmosMeteringMode = Eac3AtmosMeteringMode' Lude.Text
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

pattern EAMMItuBs17701 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17701 = Eac3AtmosMeteringMode' "ITU_BS_1770_1"

pattern EAMMItuBs17702 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17702 = Eac3AtmosMeteringMode' "ITU_BS_1770_2"

pattern EAMMItuBs17703 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17703 = Eac3AtmosMeteringMode' "ITU_BS_1770_3"

pattern EAMMItuBs17704 :: Eac3AtmosMeteringMode
pattern EAMMItuBs17704 = Eac3AtmosMeteringMode' "ITU_BS_1770_4"

pattern EAMMLeqA :: Eac3AtmosMeteringMode
pattern EAMMLeqA = Eac3AtmosMeteringMode' "LEQ_A"

{-# COMPLETE
  EAMMItuBs17701,
  EAMMItuBs17702,
  EAMMItuBs17703,
  EAMMItuBs17704,
  EAMMLeqA,
  Eac3AtmosMeteringMode'
  #-}
