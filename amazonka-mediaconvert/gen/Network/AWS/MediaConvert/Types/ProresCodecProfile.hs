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
-- Module      : Network.AWS.MediaConvert.Types.ProresCodecProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresCodecProfile
  ( ProresCodecProfile
      ( ..,
        ProresCodecProfile_APPLE_PRORES_422,
        ProresCodecProfile_APPLE_PRORES_422_HQ,
        ProresCodecProfile_APPLE_PRORES_422_LT,
        ProresCodecProfile_APPLE_PRORES_422_PROXY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Use Profile (ProResCodecProfile) to specify the type of Apple ProRes
-- codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile'
  { fromProresCodecProfile ::
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

pattern ProresCodecProfile_APPLE_PRORES_422 :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422 = ProresCodecProfile' "APPLE_PRORES_422"

pattern ProresCodecProfile_APPLE_PRORES_422_HQ :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_HQ = ProresCodecProfile' "APPLE_PRORES_422_HQ"

pattern ProresCodecProfile_APPLE_PRORES_422_LT :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_LT = ProresCodecProfile' "APPLE_PRORES_422_LT"

pattern ProresCodecProfile_APPLE_PRORES_422_PROXY :: ProresCodecProfile
pattern ProresCodecProfile_APPLE_PRORES_422_PROXY = ProresCodecProfile' "APPLE_PRORES_422_PROXY"

{-# COMPLETE
  ProresCodecProfile_APPLE_PRORES_422,
  ProresCodecProfile_APPLE_PRORES_422_HQ,
  ProresCodecProfile_APPLE_PRORES_422_LT,
  ProresCodecProfile_APPLE_PRORES_422_PROXY,
  ProresCodecProfile'
  #-}
