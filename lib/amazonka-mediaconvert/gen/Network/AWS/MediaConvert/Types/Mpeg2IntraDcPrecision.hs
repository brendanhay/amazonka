{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
  ( Mpeg2IntraDcPrecision
    ( Mpeg2IntraDcPrecision'
    , Mpeg2IntraDcPrecisionAuto
    , Mpeg2IntraDcPrecisionIntraDcPrecision8
    , Mpeg2IntraDcPrecisionIntraDcPrecision9
    , Mpeg2IntraDcPrecisionIntraDcPrecision10
    , Mpeg2IntraDcPrecisionIntraDcPrecision11
    , fromMpeg2IntraDcPrecision
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
newtype Mpeg2IntraDcPrecision = Mpeg2IntraDcPrecision'{fromMpeg2IntraDcPrecision
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern Mpeg2IntraDcPrecisionAuto :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecisionAuto = Mpeg2IntraDcPrecision' "AUTO"

pattern Mpeg2IntraDcPrecisionIntraDcPrecision8 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecisionIntraDcPrecision8 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_8"

pattern Mpeg2IntraDcPrecisionIntraDcPrecision9 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecisionIntraDcPrecision9 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_9"

pattern Mpeg2IntraDcPrecisionIntraDcPrecision10 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecisionIntraDcPrecision10 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_10"

pattern Mpeg2IntraDcPrecisionIntraDcPrecision11 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecisionIntraDcPrecision11 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_11"

{-# COMPLETE 
  Mpeg2IntraDcPrecisionAuto,

  Mpeg2IntraDcPrecisionIntraDcPrecision8,

  Mpeg2IntraDcPrecisionIntraDcPrecision9,

  Mpeg2IntraDcPrecisionIntraDcPrecision10,

  Mpeg2IntraDcPrecisionIntraDcPrecision11,
  Mpeg2IntraDcPrecision'
  #-}
