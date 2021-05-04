{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputMaximumBitrate
  ( InputMaximumBitrate
      ( ..,
        InputMaximumBitrate_MAX_10_MBPS,
        InputMaximumBitrate_MAX_20_MBPS,
        InputMaximumBitrate_MAX_50_MBPS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are
-- supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate'
  { fromInputMaximumBitrate ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
