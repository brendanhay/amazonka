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
-- Module      : Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M3u8TimedMetadataBehavior
  ( M3u8TimedMetadataBehavior
      ( ..,
        M3u8TimedMetadataBehavior_NO_PASSTHROUGH,
        M3u8TimedMetadataBehavior_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | M3u8 Timed Metadata Behavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior'
  { fromM3u8TimedMetadataBehavior ::
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

pattern M3u8TimedMetadataBehavior_NO_PASSTHROUGH :: M3u8TimedMetadataBehavior
pattern M3u8TimedMetadataBehavior_NO_PASSTHROUGH = M3u8TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern M3u8TimedMetadataBehavior_PASSTHROUGH :: M3u8TimedMetadataBehavior
pattern M3u8TimedMetadataBehavior_PASSTHROUGH = M3u8TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  M3u8TimedMetadataBehavior_NO_PASSTHROUGH,
  M3u8TimedMetadataBehavior_PASSTHROUGH,
  M3u8TimedMetadataBehavior'
  #-}
