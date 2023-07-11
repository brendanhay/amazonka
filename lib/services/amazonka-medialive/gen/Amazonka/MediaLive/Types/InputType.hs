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
-- Module      : Amazonka.MediaLive.Types.InputType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputType
  ( InputType
      ( ..,
        InputType_AWS_CDI,
        InputType_INPUT_DEVICE,
        InputType_MEDIACONNECT,
        InputType_MP4_FILE,
        InputType_RTMP_PULL,
        InputType_RTMP_PUSH,
        InputType_RTP_PUSH,
        InputType_TS_FILE,
        InputType_UDP_PUSH,
        InputType_URL_PULL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The different types of inputs that AWS Elemental MediaLive supports.
newtype InputType = InputType'
  { fromInputType ::
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

pattern InputType_AWS_CDI :: InputType
pattern InputType_AWS_CDI = InputType' "AWS_CDI"

pattern InputType_INPUT_DEVICE :: InputType
pattern InputType_INPUT_DEVICE = InputType' "INPUT_DEVICE"

pattern InputType_MEDIACONNECT :: InputType
pattern InputType_MEDIACONNECT = InputType' "MEDIACONNECT"

pattern InputType_MP4_FILE :: InputType
pattern InputType_MP4_FILE = InputType' "MP4_FILE"

pattern InputType_RTMP_PULL :: InputType
pattern InputType_RTMP_PULL = InputType' "RTMP_PULL"

pattern InputType_RTMP_PUSH :: InputType
pattern InputType_RTMP_PUSH = InputType' "RTMP_PUSH"

pattern InputType_RTP_PUSH :: InputType
pattern InputType_RTP_PUSH = InputType' "RTP_PUSH"

pattern InputType_TS_FILE :: InputType
pattern InputType_TS_FILE = InputType' "TS_FILE"

pattern InputType_UDP_PUSH :: InputType
pattern InputType_UDP_PUSH = InputType' "UDP_PUSH"

pattern InputType_URL_PULL :: InputType
pattern InputType_URL_PULL = InputType' "URL_PULL"

{-# COMPLETE
  InputType_AWS_CDI,
  InputType_INPUT_DEVICE,
  InputType_MEDIACONNECT,
  InputType_MP4_FILE,
  InputType_RTMP_PULL,
  InputType_RTMP_PUSH,
  InputType_RTP_PUSH,
  InputType_TS_FILE,
  InputType_UDP_PUSH,
  InputType_URL_PULL,
  InputType'
  #-}
