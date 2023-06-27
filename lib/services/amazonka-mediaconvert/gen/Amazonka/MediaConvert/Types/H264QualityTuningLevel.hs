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
-- Module      : Amazonka.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264QualityTuningLevel
  ( H264QualityTuningLevel
      ( ..,
        H264QualityTuningLevel_MULTI_PASS_HQ,
        H264QualityTuningLevel_SINGLE_PASS,
        H264QualityTuningLevel_SINGLE_PASS_HQ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Quality tuning level you choose represents a trade-off between the
-- encoding speed of your job and the output video quality. For the fastest
-- encoding speed at the cost of video quality: Choose Single pass. For a
-- good balance between encoding speed and video quality: Leave blank or
-- keep the default value Single pass HQ. For the best video quality, at
-- the cost of encoding speed: Choose Multi pass HQ. MediaConvert performs
-- an analysis pass on your input followed by an encoding pass. Outputs
-- that use this feature incur pro-tier pricing.
newtype H264QualityTuningLevel = H264QualityTuningLevel'
  { fromH264QualityTuningLevel ::
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

pattern H264QualityTuningLevel_MULTI_PASS_HQ :: H264QualityTuningLevel
pattern H264QualityTuningLevel_MULTI_PASS_HQ = H264QualityTuningLevel' "MULTI_PASS_HQ"

pattern H264QualityTuningLevel_SINGLE_PASS :: H264QualityTuningLevel
pattern H264QualityTuningLevel_SINGLE_PASS = H264QualityTuningLevel' "SINGLE_PASS"

pattern H264QualityTuningLevel_SINGLE_PASS_HQ :: H264QualityTuningLevel
pattern H264QualityTuningLevel_SINGLE_PASS_HQ = H264QualityTuningLevel' "SINGLE_PASS_HQ"

{-# COMPLETE
  H264QualityTuningLevel_MULTI_PASS_HQ,
  H264QualityTuningLevel_SINGLE_PASS,
  H264QualityTuningLevel_SINGLE_PASS_HQ,
  H264QualityTuningLevel'
  #-}
