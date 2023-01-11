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
-- Module      : Amazonka.MediaConvert.Types.CmafPtsOffsetHandlingForBFrames
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafPtsOffsetHandlingForBFrames
  ( CmafPtsOffsetHandlingForBFrames
      ( ..,
        CmafPtsOffsetHandlingForBFrames_MATCH_INITIAL_PTS,
        CmafPtsOffsetHandlingForBFrames_ZERO_BASED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting only when your output video stream has B-frames, which
-- causes the initial presentation time stamp (PTS) to be offset from the
-- initial decode time stamp (DTS). Specify how MediaConvert handles PTS
-- when writing time stamps in output DASH manifests. Choose Match initial
-- PTS (MATCH_INITIAL_PTS) when you want MediaConvert to use the initial
-- PTS as the first time stamp in the manifest. Choose Zero-based
-- (ZERO_BASED) to have MediaConvert ignore the initial PTS in the video
-- stream and instead write the initial time stamp as zero in the manifest.
-- For outputs that don\'t have B-frames, the time stamps in your DASH
-- manifests start at zero regardless of your choice here.
newtype CmafPtsOffsetHandlingForBFrames = CmafPtsOffsetHandlingForBFrames'
  { fromCmafPtsOffsetHandlingForBFrames ::
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

pattern CmafPtsOffsetHandlingForBFrames_MATCH_INITIAL_PTS :: CmafPtsOffsetHandlingForBFrames
pattern CmafPtsOffsetHandlingForBFrames_MATCH_INITIAL_PTS = CmafPtsOffsetHandlingForBFrames' "MATCH_INITIAL_PTS"

pattern CmafPtsOffsetHandlingForBFrames_ZERO_BASED :: CmafPtsOffsetHandlingForBFrames
pattern CmafPtsOffsetHandlingForBFrames_ZERO_BASED = CmafPtsOffsetHandlingForBFrames' "ZERO_BASED"

{-# COMPLETE
  CmafPtsOffsetHandlingForBFrames_MATCH_INITIAL_PTS,
  CmafPtsOffsetHandlingForBFrames_ZERO_BASED,
  CmafPtsOffsetHandlingForBFrames'
  #-}
