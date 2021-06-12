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
-- Module      : Network.AWS.MediaConvert.Types.M3u8PcrControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M3u8PcrControl
  ( M3u8PcrControl
      ( ..,
        M3u8PcrControl_CONFIGURED_PCR_PERIOD,
        M3u8PcrControl_PCR_EVERY_PES_PACKET
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is
-- inserted for every Packetized Elementary Stream (PES) header. This
-- parameter is effective only when the PCR PID is the same as the video or
-- audio elementary stream.
newtype M3u8PcrControl = M3u8PcrControl'
  { fromM3u8PcrControl ::
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

pattern M3u8PcrControl_CONFIGURED_PCR_PERIOD :: M3u8PcrControl
pattern M3u8PcrControl_CONFIGURED_PCR_PERIOD = M3u8PcrControl' "CONFIGURED_PCR_PERIOD"

pattern M3u8PcrControl_PCR_EVERY_PES_PACKET :: M3u8PcrControl
pattern M3u8PcrControl_PCR_EVERY_PES_PACKET = M3u8PcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  M3u8PcrControl_CONFIGURED_PCR_PERIOD,
  M3u8PcrControl_PCR_EVERY_PES_PACKET,
  M3u8PcrControl'
  #-}
