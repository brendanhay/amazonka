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
-- Module      : Amazonka.KinesisVideo.Types.ChannelProtocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ChannelProtocol
  ( ChannelProtocol
      ( ..,
        ChannelProtocol_HTTPS,
        ChannelProtocol_WEBRTC,
        ChannelProtocol_WSS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChannelProtocol = ChannelProtocol'
  { fromChannelProtocol ::
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

pattern ChannelProtocol_HTTPS :: ChannelProtocol
pattern ChannelProtocol_HTTPS = ChannelProtocol' "HTTPS"

pattern ChannelProtocol_WEBRTC :: ChannelProtocol
pattern ChannelProtocol_WEBRTC = ChannelProtocol' "WEBRTC"

pattern ChannelProtocol_WSS :: ChannelProtocol
pattern ChannelProtocol_WSS = ChannelProtocol' "WSS"

{-# COMPLETE
  ChannelProtocol_HTTPS,
  ChannelProtocol_WEBRTC,
  ChannelProtocol_WSS,
  ChannelProtocol'
  #-}
