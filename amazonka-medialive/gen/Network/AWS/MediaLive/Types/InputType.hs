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
-- Module      : Network.AWS.MediaLive.Types.InputType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputType
  ( InputType
      ( ..,
        InputType_AWS_CDI,
        InputType_INPUT_DEVICE,
        InputType_MEDIACONNECT,
        InputType_MP4_FILE,
        InputType_RTMP_PULL,
        InputType_RTMP_PUSH,
        InputType_RTP_PUSH,
        InputType_UDP_PUSH,
        InputType_URL_PULL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for InputType
newtype InputType = InputType'
  { fromInputType ::
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
  InputType_UDP_PUSH,
  InputType_URL_PULL,
  InputType'
  #-}
