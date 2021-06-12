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
-- Module      : Network.AWS.MediaConvert.Types.HlsStreamInfResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsStreamInfResolution
  ( HlsStreamInfResolution
      ( ..,
        HlsStreamInfResolution_EXCLUDE,
        HlsStreamInfResolution_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
newtype HlsStreamInfResolution = HlsStreamInfResolution'
  { fromHlsStreamInfResolution ::
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

pattern HlsStreamInfResolution_EXCLUDE :: HlsStreamInfResolution
pattern HlsStreamInfResolution_EXCLUDE = HlsStreamInfResolution' "EXCLUDE"

pattern HlsStreamInfResolution_INCLUDE :: HlsStreamInfResolution
pattern HlsStreamInfResolution_INCLUDE = HlsStreamInfResolution' "INCLUDE"

{-# COMPLETE
  HlsStreamInfResolution_EXCLUDE,
  HlsStreamInfResolution_INCLUDE,
  HlsStreamInfResolution'
  #-}
