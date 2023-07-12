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
-- Module      : Amazonka.MediaConvert.Types.DashIsoGroupAudioChannelConfigSchemeIdUri
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashIsoGroupAudioChannelConfigSchemeIdUri
  ( DashIsoGroupAudioChannelConfigSchemeIdUri
      ( ..,
        DashIsoGroupAudioChannelConfigSchemeIdUri_DOLBY_CHANNEL_CONFIGURATION,
        DashIsoGroupAudioChannelConfigSchemeIdUri_MPEG_CHANNEL_CONFIGURATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting only when your audio codec is a Dolby one (AC3, EAC3,
-- or Atmos) and your downstream workflow requires that your DASH manifest
-- use the Dolby channel configuration tag, rather than the MPEG one. For
-- example, you might need to use this to make dynamic ad insertion work.
-- Specify which audio channel configuration scheme ID URI MediaConvert
-- writes in your DASH manifest. Keep the default value, MPEG channel
-- configuration (MPEG_CHANNEL_CONFIGURATION), to have MediaConvert write
-- this: urn:mpeg:mpegB:cicp:ChannelConfiguration. Choose Dolby channel
-- configuration (DOLBY_CHANNEL_CONFIGURATION) to have MediaConvert write
-- this instead: tag:dolby.com,2014:dash:audio_channel_configuration:2011.
newtype DashIsoGroupAudioChannelConfigSchemeIdUri = DashIsoGroupAudioChannelConfigSchemeIdUri'
  { fromDashIsoGroupAudioChannelConfigSchemeIdUri ::
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

pattern DashIsoGroupAudioChannelConfigSchemeIdUri_DOLBY_CHANNEL_CONFIGURATION :: DashIsoGroupAudioChannelConfigSchemeIdUri
pattern DashIsoGroupAudioChannelConfigSchemeIdUri_DOLBY_CHANNEL_CONFIGURATION = DashIsoGroupAudioChannelConfigSchemeIdUri' "DOLBY_CHANNEL_CONFIGURATION"

pattern DashIsoGroupAudioChannelConfigSchemeIdUri_MPEG_CHANNEL_CONFIGURATION :: DashIsoGroupAudioChannelConfigSchemeIdUri
pattern DashIsoGroupAudioChannelConfigSchemeIdUri_MPEG_CHANNEL_CONFIGURATION = DashIsoGroupAudioChannelConfigSchemeIdUri' "MPEG_CHANNEL_CONFIGURATION"

{-# COMPLETE
  DashIsoGroupAudioChannelConfigSchemeIdUri_DOLBY_CHANNEL_CONFIGURATION,
  DashIsoGroupAudioChannelConfigSchemeIdUri_MPEG_CHANNEL_CONFIGURATION,
  DashIsoGroupAudioChannelConfigSchemeIdUri'
  #-}
