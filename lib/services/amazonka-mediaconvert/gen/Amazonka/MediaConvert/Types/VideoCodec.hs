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
-- Module      : Amazonka.MediaConvert.Types.VideoCodec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoCodec
  ( VideoCodec
      ( ..,
        VideoCodec_AV1,
        VideoCodec_AVC_INTRA,
        VideoCodec_FRAME_CAPTURE,
        VideoCodec_H_264,
        VideoCodec_H_265,
        VideoCodec_MPEG2,
        VideoCodec_PRORES,
        VideoCodec_VC3,
        VideoCodec_VP8,
        VideoCodec_VP9,
        VideoCodec_XAVC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Type of video codec
newtype VideoCodec = VideoCodec'
  { fromVideoCodec ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern VideoCodec_AV1 :: VideoCodec
pattern VideoCodec_AV1 = VideoCodec' "AV1"

pattern VideoCodec_AVC_INTRA :: VideoCodec
pattern VideoCodec_AVC_INTRA = VideoCodec' "AVC_INTRA"

pattern VideoCodec_FRAME_CAPTURE :: VideoCodec
pattern VideoCodec_FRAME_CAPTURE = VideoCodec' "FRAME_CAPTURE"

pattern VideoCodec_H_264 :: VideoCodec
pattern VideoCodec_H_264 = VideoCodec' "H_264"

pattern VideoCodec_H_265 :: VideoCodec
pattern VideoCodec_H_265 = VideoCodec' "H_265"

pattern VideoCodec_MPEG2 :: VideoCodec
pattern VideoCodec_MPEG2 = VideoCodec' "MPEG2"

pattern VideoCodec_PRORES :: VideoCodec
pattern VideoCodec_PRORES = VideoCodec' "PRORES"

pattern VideoCodec_VC3 :: VideoCodec
pattern VideoCodec_VC3 = VideoCodec' "VC3"

pattern VideoCodec_VP8 :: VideoCodec
pattern VideoCodec_VP8 = VideoCodec' "VP8"

pattern VideoCodec_VP9 :: VideoCodec
pattern VideoCodec_VP9 = VideoCodec' "VP9"

pattern VideoCodec_XAVC :: VideoCodec
pattern VideoCodec_XAVC = VideoCodec' "XAVC"

{-# COMPLETE
  VideoCodec_AV1,
  VideoCodec_AVC_INTRA,
  VideoCodec_FRAME_CAPTURE,
  VideoCodec_H_264,
  VideoCodec_H_265,
  VideoCodec_MPEG2,
  VideoCodec_PRORES,
  VideoCodec_VC3,
  VideoCodec_VP8,
  VideoCodec_VP9,
  VideoCodec_XAVC,
  VideoCodec'
  #-}
