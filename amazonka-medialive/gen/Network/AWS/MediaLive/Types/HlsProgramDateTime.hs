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
-- Module      : Network.AWS.MediaLive.Types.HlsProgramDateTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsProgramDateTime
  ( HlsProgramDateTime
      ( ..,
        HlsProgramDateTime_EXCLUDE,
        HlsProgramDateTime_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Program Date Time
newtype HlsProgramDateTime = HlsProgramDateTime'
  { fromHlsProgramDateTime ::
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

pattern HlsProgramDateTime_EXCLUDE :: HlsProgramDateTime
pattern HlsProgramDateTime_EXCLUDE = HlsProgramDateTime' "EXCLUDE"

pattern HlsProgramDateTime_INCLUDE :: HlsProgramDateTime
pattern HlsProgramDateTime_INCLUDE = HlsProgramDateTime' "INCLUDE"

{-# COMPLETE
  HlsProgramDateTime_EXCLUDE,
  HlsProgramDateTime_INCLUDE,
  HlsProgramDateTime'
  #-}
