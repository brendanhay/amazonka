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
        UdpPush,
        RtpPush,
        RtmpPush,
        RtmpPull,
        URLPull,
        MP4File,
        Mediaconnect,
        InputDevice,
        AWSCdi
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

pattern UdpPush :: InputType
pattern UdpPush = InputType' "UDP_PUSH"

pattern RtpPush :: InputType
pattern RtpPush = InputType' "RTP_PUSH"

pattern RtmpPush :: InputType
pattern RtmpPush = InputType' "RTMP_PUSH"

pattern RtmpPull :: InputType
pattern RtmpPull = InputType' "RTMP_PULL"

pattern URLPull :: InputType
pattern URLPull = InputType' "URL_PULL"

pattern MP4File :: InputType
pattern MP4File = InputType' "MP4_FILE"

pattern Mediaconnect :: InputType
pattern Mediaconnect = InputType' "MEDIACONNECT"

pattern InputDevice :: InputType
pattern InputDevice = InputType' "INPUT_DEVICE"

pattern AWSCdi :: InputType
pattern AWSCdi = InputType' "AWS_CDI"

{-# COMPLETE
  UdpPush,
  RtpPush,
  RtmpPush,
  RtmpPull,
  URLPull,
  MP4File,
  Mediaconnect,
  InputDevice,
  AWSCdi,
  InputType'
  #-}
