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
-- Module      : Network.AWS.MediaLive.Types.H264QualityLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264QualityLevel
  ( H264QualityLevel
      ( ..,
        H264QualityLevel_ENHANCED_QUALITY,
        H264QualityLevel_STANDARD_QUALITY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Quality Level
newtype H264QualityLevel = H264QualityLevel'
  { fromH264QualityLevel ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern H264QualityLevel_ENHANCED_QUALITY :: H264QualityLevel
pattern H264QualityLevel_ENHANCED_QUALITY = H264QualityLevel' "ENHANCED_QUALITY"

pattern H264QualityLevel_STANDARD_QUALITY :: H264QualityLevel
pattern H264QualityLevel_STANDARD_QUALITY = H264QualityLevel' "STANDARD_QUALITY"

{-# COMPLETE
  H264QualityLevel_ENHANCED_QUALITY,
  H264QualityLevel_STANDARD_QUALITY,
  H264QualityLevel'
  #-}
