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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
  ( DASHPlaybackMode
      ( ..,
        DASHPlaybackMode_LIVE,
        DASHPlaybackMode_LIVE_REPLAY,
        DASHPlaybackMode_ON_DEMAND
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DASHPlaybackMode = DASHPlaybackMode'
  { fromDASHPlaybackMode ::
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

pattern DASHPlaybackMode_LIVE :: DASHPlaybackMode
pattern DASHPlaybackMode_LIVE = DASHPlaybackMode' "LIVE"

pattern DASHPlaybackMode_LIVE_REPLAY :: DASHPlaybackMode
pattern DASHPlaybackMode_LIVE_REPLAY = DASHPlaybackMode' "LIVE_REPLAY"

pattern DASHPlaybackMode_ON_DEMAND :: DASHPlaybackMode
pattern DASHPlaybackMode_ON_DEMAND = DASHPlaybackMode' "ON_DEMAND"

{-# COMPLETE
  DASHPlaybackMode_LIVE,
  DASHPlaybackMode_LIVE_REPLAY,
  DASHPlaybackMode_ON_DEMAND,
  DASHPlaybackMode'
  #-}
