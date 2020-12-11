-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
  ( Mpeg2IntraDcPrecision
      ( Mpeg2IntraDcPrecision',
        MIDPAuto,
        MIDPIntraDcPrecision10,
        MIDPIntraDcPrecision11,
        MIDPIntraDcPrecision8,
        MIDPIntraDcPrecision9
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
newtype Mpeg2IntraDcPrecision = Mpeg2IntraDcPrecision' Lude.Text
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

pattern MIDPAuto :: Mpeg2IntraDcPrecision
pattern MIDPAuto = Mpeg2IntraDcPrecision' "AUTO"

pattern MIDPIntraDcPrecision10 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision10 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_10"

pattern MIDPIntraDcPrecision11 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision11 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_11"

pattern MIDPIntraDcPrecision8 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision8 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_8"

pattern MIDPIntraDcPrecision9 :: Mpeg2IntraDcPrecision
pattern MIDPIntraDcPrecision9 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_9"

{-# COMPLETE
  MIDPAuto,
  MIDPIntraDcPrecision10,
  MIDPIntraDcPrecision11,
  MIDPIntraDcPrecision8,
  MIDPIntraDcPrecision9,
  Mpeg2IntraDcPrecision'
  #-}
