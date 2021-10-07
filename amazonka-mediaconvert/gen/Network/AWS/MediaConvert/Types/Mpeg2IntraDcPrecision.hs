{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2IntraDcPrecision
  ( Mpeg2IntraDcPrecision
      ( ..,
        Mpeg2IntraDcPrecision_AUTO,
        Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_10,
        Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_11,
        Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_8,
        Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_9
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization
-- precision for intra-block DC coefficients. If you choose the value auto,
-- the service will automatically select the precision based on the
-- per-frame compression ratio.
newtype Mpeg2IntraDcPrecision = Mpeg2IntraDcPrecision'
  { fromMpeg2IntraDcPrecision ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Mpeg2IntraDcPrecision_AUTO :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecision_AUTO = Mpeg2IntraDcPrecision' "AUTO"

pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_10 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_10 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_10"

pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_11 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_11 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_11"

pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_8 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_8 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_8"

pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_9 :: Mpeg2IntraDcPrecision
pattern Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_9 = Mpeg2IntraDcPrecision' "INTRA_DC_PRECISION_9"

{-# COMPLETE
  Mpeg2IntraDcPrecision_AUTO,
  Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_10,
  Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_11,
  Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_8,
  Mpeg2IntraDcPrecision_INTRA_DC_PRECISION_9,
  Mpeg2IntraDcPrecision'
  #-}
