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
-- Module      : Amazonka.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
  ( DashIsoPlaybackDeviceCompatibility
      ( ..,
        DashIsoPlaybackDeviceCompatibility_CENC_V1,
        DashIsoPlaybackDeviceCompatibility_UNENCRYPTED_SEI
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | This setting can improve the compatibility of your output with video
-- players on obsolete devices. It applies only to DASH H.264 outputs with
-- DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct
-- problems with playback on older devices. Otherwise, keep the default
-- setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that
-- output, the service will exclude the access unit delimiter and will
-- leave the SEI NAL units unencrypted.
newtype DashIsoPlaybackDeviceCompatibility = DashIsoPlaybackDeviceCompatibility'
  { fromDashIsoPlaybackDeviceCompatibility ::
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

pattern DashIsoPlaybackDeviceCompatibility_CENC_V1 :: DashIsoPlaybackDeviceCompatibility
pattern DashIsoPlaybackDeviceCompatibility_CENC_V1 = DashIsoPlaybackDeviceCompatibility' "CENC_V1"

pattern DashIsoPlaybackDeviceCompatibility_UNENCRYPTED_SEI :: DashIsoPlaybackDeviceCompatibility
pattern DashIsoPlaybackDeviceCompatibility_UNENCRYPTED_SEI = DashIsoPlaybackDeviceCompatibility' "UNENCRYPTED_SEI"

{-# COMPLETE
  DashIsoPlaybackDeviceCompatibility_CENC_V1,
  DashIsoPlaybackDeviceCompatibility_UNENCRYPTED_SEI,
  DashIsoPlaybackDeviceCompatibility'
  #-}
