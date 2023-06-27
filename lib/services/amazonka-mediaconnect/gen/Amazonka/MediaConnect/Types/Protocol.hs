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
-- Module      : Amazonka.MediaConnect.Types.Protocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Protocol
  ( Protocol
      ( ..,
        Protocol_Cdi,
        Protocol_Fujitsu_qos,
        Protocol_Rist,
        Protocol_Rtp,
        Protocol_Rtp_fec,
        Protocol_Srt_caller,
        Protocol_Srt_listener,
        Protocol_St2110_jpegxs,
        Protocol_Udp,
        Protocol_Zixi_pull,
        Protocol_Zixi_push
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Protocol = Protocol'
  { fromProtocol ::
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

pattern Protocol_Cdi :: Protocol
pattern Protocol_Cdi = Protocol' "cdi"

pattern Protocol_Fujitsu_qos :: Protocol
pattern Protocol_Fujitsu_qos = Protocol' "fujitsu-qos"

pattern Protocol_Rist :: Protocol
pattern Protocol_Rist = Protocol' "rist"

pattern Protocol_Rtp :: Protocol
pattern Protocol_Rtp = Protocol' "rtp"

pattern Protocol_Rtp_fec :: Protocol
pattern Protocol_Rtp_fec = Protocol' "rtp-fec"

pattern Protocol_Srt_caller :: Protocol
pattern Protocol_Srt_caller = Protocol' "srt-caller"

pattern Protocol_Srt_listener :: Protocol
pattern Protocol_Srt_listener = Protocol' "srt-listener"

pattern Protocol_St2110_jpegxs :: Protocol
pattern Protocol_St2110_jpegxs = Protocol' "st2110-jpegxs"

pattern Protocol_Udp :: Protocol
pattern Protocol_Udp = Protocol' "udp"

pattern Protocol_Zixi_pull :: Protocol
pattern Protocol_Zixi_pull = Protocol' "zixi-pull"

pattern Protocol_Zixi_push :: Protocol
pattern Protocol_Zixi_push = Protocol' "zixi-push"

{-# COMPLETE
  Protocol_Cdi,
  Protocol_Fujitsu_qos,
  Protocol_Rist,
  Protocol_Rtp,
  Protocol_Rtp_fec,
  Protocol_Srt_caller,
  Protocol_Srt_listener,
  Protocol_St2110_jpegxs,
  Protocol_Udp,
  Protocol_Zixi_pull,
  Protocol_Zixi_push,
  Protocol'
  #-}
