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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2QualityTuningLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2QualityTuningLevel
  ( Mpeg2QualityTuningLevel
      ( ..,
        Mpeg2QualityTuningLevel_MULTI_PASS,
        Mpeg2QualityTuningLevel_SINGLE_PASS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how
-- you want to trade off encoding speed for output video quality. The
-- default behavior is faster, lower quality, single-pass encoding.
newtype Mpeg2QualityTuningLevel = Mpeg2QualityTuningLevel'
  { fromMpeg2QualityTuningLevel ::
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

pattern Mpeg2QualityTuningLevel_MULTI_PASS :: Mpeg2QualityTuningLevel
pattern Mpeg2QualityTuningLevel_MULTI_PASS = Mpeg2QualityTuningLevel' "MULTI_PASS"

pattern Mpeg2QualityTuningLevel_SINGLE_PASS :: Mpeg2QualityTuningLevel
pattern Mpeg2QualityTuningLevel_SINGLE_PASS = Mpeg2QualityTuningLevel' "SINGLE_PASS"

{-# COMPLETE
  Mpeg2QualityTuningLevel_MULTI_PASS,
  Mpeg2QualityTuningLevel_SINGLE_PASS,
  Mpeg2QualityTuningLevel'
  #-}
