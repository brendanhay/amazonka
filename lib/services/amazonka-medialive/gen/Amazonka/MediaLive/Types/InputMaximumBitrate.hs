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
-- Module      : Amazonka.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputMaximumBitrate
  ( InputMaximumBitrate
      ( ..,
        InputMaximumBitrate_MAX_10_MBPS,
        InputMaximumBitrate_MAX_20_MBPS,
        InputMaximumBitrate_MAX_50_MBPS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are
-- supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate'
  { fromInputMaximumBitrate ::
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

pattern InputMaximumBitrate_MAX_10_MBPS :: InputMaximumBitrate
pattern InputMaximumBitrate_MAX_10_MBPS = InputMaximumBitrate' "MAX_10_MBPS"

pattern InputMaximumBitrate_MAX_20_MBPS :: InputMaximumBitrate
pattern InputMaximumBitrate_MAX_20_MBPS = InputMaximumBitrate' "MAX_20_MBPS"

pattern InputMaximumBitrate_MAX_50_MBPS :: InputMaximumBitrate
pattern InputMaximumBitrate_MAX_50_MBPS = InputMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE
  InputMaximumBitrate_MAX_10_MBPS,
  InputMaximumBitrate_MAX_20_MBPS,
  InputMaximumBitrate_MAX_50_MBPS,
  InputMaximumBitrate'
  #-}
