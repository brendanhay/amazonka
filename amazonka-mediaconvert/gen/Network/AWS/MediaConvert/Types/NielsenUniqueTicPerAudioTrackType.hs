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
-- Module      : Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
  ( NielsenUniqueTicPerAudioTrackType
      ( ..,
        NielsenUniqueTicPerAudioTrackType_RESERVE_UNIQUE_TICS_PER_TRACK,
        NielsenUniqueTicPerAudioTrackType_SAME_TICS_PER_TRACK
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | To create assets that have the same TIC values in each audio track, keep
-- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
-- that have unique TIC values for each audio track, choose Use unique TICs
-- (RESERVE_UNIQUE_TICS_PER_TRACK).
newtype NielsenUniqueTicPerAudioTrackType = NielsenUniqueTicPerAudioTrackType'
  { fromNielsenUniqueTicPerAudioTrackType ::
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

pattern NielsenUniqueTicPerAudioTrackType_RESERVE_UNIQUE_TICS_PER_TRACK :: NielsenUniqueTicPerAudioTrackType
pattern NielsenUniqueTicPerAudioTrackType_RESERVE_UNIQUE_TICS_PER_TRACK = NielsenUniqueTicPerAudioTrackType' "RESERVE_UNIQUE_TICS_PER_TRACK"

pattern NielsenUniqueTicPerAudioTrackType_SAME_TICS_PER_TRACK :: NielsenUniqueTicPerAudioTrackType
pattern NielsenUniqueTicPerAudioTrackType_SAME_TICS_PER_TRACK = NielsenUniqueTicPerAudioTrackType' "SAME_TICS_PER_TRACK"

{-# COMPLETE
  NielsenUniqueTicPerAudioTrackType_RESERVE_UNIQUE_TICS_PER_TRACK,
  NielsenUniqueTicPerAudioTrackType_SAME_TICS_PER_TRACK,
  NielsenUniqueTicPerAudioTrackType'
  #-}
