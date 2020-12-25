{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputType
  ( InputType
      ( InputType',
        InputTypeUdpPush,
        InputTypeRtpPush,
        InputTypeRtmpPush,
        InputTypeRtmpPull,
        InputTypeUrlPull,
        InputTypeMP4File,
        InputTypeMediaconnect,
        InputTypeInputDevice,
        InputTypeAwsCdi,
        fromInputType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for InputType
newtype InputType = InputType' {fromInputType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern InputTypeUdpPush :: InputType
pattern InputTypeUdpPush = InputType' "UDP_PUSH"

pattern InputTypeRtpPush :: InputType
pattern InputTypeRtpPush = InputType' "RTP_PUSH"

pattern InputTypeRtmpPush :: InputType
pattern InputTypeRtmpPush = InputType' "RTMP_PUSH"

pattern InputTypeRtmpPull :: InputType
pattern InputTypeRtmpPull = InputType' "RTMP_PULL"

pattern InputTypeUrlPull :: InputType
pattern InputTypeUrlPull = InputType' "URL_PULL"

pattern InputTypeMP4File :: InputType
pattern InputTypeMP4File = InputType' "MP4_FILE"

pattern InputTypeMediaconnect :: InputType
pattern InputTypeMediaconnect = InputType' "MEDIACONNECT"

pattern InputTypeInputDevice :: InputType
pattern InputTypeInputDevice = InputType' "INPUT_DEVICE"

pattern InputTypeAwsCdi :: InputType
pattern InputTypeAwsCdi = InputType' "AWS_CDI"

{-# COMPLETE
  InputTypeUdpPush,
  InputTypeRtpPush,
  InputTypeRtmpPush,
  InputTypeRtmpPull,
  InputTypeUrlPull,
  InputTypeMP4File,
  InputTypeMediaconnect,
  InputTypeInputDevice,
  InputTypeAwsCdi,
  InputType'
  #-}
