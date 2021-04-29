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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
  ( HLSPlaybackMode
      ( ..,
        HLSPlaybackMode_LIVE,
        HLSPlaybackMode_LIVE_REPLAY,
        HLSPlaybackMode_ON_DEMAND
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype HLSPlaybackMode = HLSPlaybackMode'
  { fromHLSPlaybackMode ::
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
