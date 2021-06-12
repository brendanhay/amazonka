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
-- Module      : Network.AWS.MediaConvert.Types.CmafStreamInfResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafStreamInfResolution
  ( CmafStreamInfResolution
      ( ..,
        CmafStreamInfResolution_EXCLUDE,
        CmafStreamInfResolution_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
newtype CmafStreamInfResolution = CmafStreamInfResolution'
  { fromCmafStreamInfResolution ::
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

pattern CmafStreamInfResolution_EXCLUDE :: CmafStreamInfResolution
pattern CmafStreamInfResolution_EXCLUDE = CmafStreamInfResolution' "EXCLUDE"

pattern CmafStreamInfResolution_INCLUDE :: CmafStreamInfResolution
pattern CmafStreamInfResolution_INCLUDE = CmafStreamInfResolution' "INCLUDE"

{-# COMPLETE
  CmafStreamInfResolution_EXCLUDE,
  CmafStreamInfResolution_INCLUDE,
  CmafStreamInfResolution'
  #-}
