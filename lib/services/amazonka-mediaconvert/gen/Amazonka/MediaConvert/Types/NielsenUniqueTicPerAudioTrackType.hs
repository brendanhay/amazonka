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
-- Module      : Amazonka.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
  ( NielsenUniqueTicPerAudioTrackType
      ( ..,
        NielsenUniqueTicPerAudioTrackType_RESERVE_UNIQUE_TICS_PER_TRACK,
        NielsenUniqueTicPerAudioTrackType_SAME_TICS_PER_TRACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To create assets that have the same TIC values in each audio track, keep
-- the default value Share TICs (SAME_TICS_PER_TRACK). To create assets
-- that have unique TIC values for each audio track, choose Use unique TICs
-- (RESERVE_UNIQUE_TICS_PER_TRACK).
newtype NielsenUniqueTicPerAudioTrackType = NielsenUniqueTicPerAudioTrackType'
  { fromNielsenUniqueTicPerAudioTrackType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
