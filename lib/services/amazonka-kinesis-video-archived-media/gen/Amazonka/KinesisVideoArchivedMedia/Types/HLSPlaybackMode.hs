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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
  ( HLSPlaybackMode
      ( ..,
        HLSPlaybackMode_LIVE,
        HLSPlaybackMode_LIVE_REPLAY,
        HLSPlaybackMode_ON_DEMAND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HLSPlaybackMode = HLSPlaybackMode'
  { fromHLSPlaybackMode ::
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

pattern HLSPlaybackMode_LIVE :: HLSPlaybackMode
pattern HLSPlaybackMode_LIVE = HLSPlaybackMode' "LIVE"

pattern HLSPlaybackMode_LIVE_REPLAY :: HLSPlaybackMode
pattern HLSPlaybackMode_LIVE_REPLAY = HLSPlaybackMode' "LIVE_REPLAY"

pattern HLSPlaybackMode_ON_DEMAND :: HLSPlaybackMode
pattern HLSPlaybackMode_ON_DEMAND = HLSPlaybackMode' "ON_DEMAND"

{-# COMPLETE
  HLSPlaybackMode_LIVE,
  HLSPlaybackMode_LIVE_REPLAY,
  HLSPlaybackMode_ON_DEMAND,
  HLSPlaybackMode'
  #-}
