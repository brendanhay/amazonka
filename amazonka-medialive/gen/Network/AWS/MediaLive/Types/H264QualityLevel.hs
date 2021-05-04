{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | H264 Quality Level
newtype H264QualityLevel = H264QualityLevel'
  { fromH264QualityLevel ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
