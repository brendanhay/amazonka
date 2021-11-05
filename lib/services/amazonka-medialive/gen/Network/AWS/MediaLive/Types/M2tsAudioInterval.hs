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
-- Module      : Amazonka.MediaLive.Types.M2tsAudioInterval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsAudioInterval
  ( M2tsAudioInterval
      ( ..,
        M2tsAudioInterval_VIDEO_AND_FIXED_INTERVALS,
        M2tsAudioInterval_VIDEO_INTERVAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | M2ts Audio Interval
newtype M2tsAudioInterval = M2tsAudioInterval'
  { fromM2tsAudioInterval ::
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

pattern M2tsAudioInterval_VIDEO_AND_FIXED_INTERVALS :: M2tsAudioInterval
pattern M2tsAudioInterval_VIDEO_AND_FIXED_INTERVALS = M2tsAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern M2tsAudioInterval_VIDEO_INTERVAL :: M2tsAudioInterval
pattern M2tsAudioInterval_VIDEO_INTERVAL = M2tsAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE
  M2tsAudioInterval_VIDEO_AND_FIXED_INTERVALS,
  M2tsAudioInterval_VIDEO_INTERVAL,
  M2tsAudioInterval'
  #-}
