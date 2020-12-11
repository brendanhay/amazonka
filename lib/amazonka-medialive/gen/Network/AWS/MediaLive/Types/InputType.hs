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
        AWSCdi,
        InputDevice,
        MP4File,
        Mediaconnect,
        RtmpPull,
        RtmpPush,
        RtpPush,
        URLPull,
        UdpPush
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for InputType
newtype InputType = InputType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AWSCdi :: InputType
pattern AWSCdi = InputType' "AWS_CDI"

pattern InputDevice :: InputType
pattern InputDevice = InputType' "INPUT_DEVICE"

pattern MP4File :: InputType
pattern MP4File = InputType' "MP4_FILE"

pattern Mediaconnect :: InputType
pattern Mediaconnect = InputType' "MEDIACONNECT"

pattern RtmpPull :: InputType
pattern RtmpPull = InputType' "RTMP_PULL"

pattern RtmpPush :: InputType
pattern RtmpPush = InputType' "RTMP_PUSH"

pattern RtpPush :: InputType
pattern RtpPush = InputType' "RTP_PUSH"

pattern URLPull :: InputType
pattern URLPull = InputType' "URL_PULL"

pattern UdpPush :: InputType
pattern UdpPush = InputType' "UDP_PUSH"

{-# COMPLETE
  AWSCdi,
  InputDevice,
  MP4File,
  Mediaconnect,
  RtmpPull,
  RtmpPush,
  RtpPush,
  URLPull,
  UdpPush,
  InputType'
  #-}
