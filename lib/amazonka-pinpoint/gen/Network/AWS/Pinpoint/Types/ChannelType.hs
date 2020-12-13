{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelType
  ( ChannelType
      ( ChannelType',
        Push,
        GCM,
        APNS,
        APNSSandbox,
        APNSVoip,
        APNSVoipSandbox,
        ADM,
        Sms,
        Voice,
        Email,
        Baidu,
        Custom
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChannelType = ChannelType' Lude.Text
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

pattern Push :: ChannelType
pattern Push = ChannelType' "PUSH"

pattern GCM :: ChannelType
pattern GCM = ChannelType' "GCM"

pattern APNS :: ChannelType
pattern APNS = ChannelType' "APNS"

pattern APNSSandbox :: ChannelType
pattern APNSSandbox = ChannelType' "APNS_SANDBOX"

pattern APNSVoip :: ChannelType
pattern APNSVoip = ChannelType' "APNS_VOIP"

pattern APNSVoipSandbox :: ChannelType
pattern APNSVoipSandbox = ChannelType' "APNS_VOIP_SANDBOX"

pattern ADM :: ChannelType
pattern ADM = ChannelType' "ADM"

pattern Sms :: ChannelType
pattern Sms = ChannelType' "SMS"

pattern Voice :: ChannelType
pattern Voice = ChannelType' "VOICE"

pattern Email :: ChannelType
pattern Email = ChannelType' "EMAIL"

pattern Baidu :: ChannelType
pattern Baidu = ChannelType' "BAIDU"

pattern Custom :: ChannelType
pattern Custom = ChannelType' "CUSTOM"

{-# COMPLETE
  Push,
  GCM,
  APNS,
  APNSSandbox,
  APNSVoip,
  APNSVoipSandbox,
  ADM,
  Sms,
  Voice,
  Email,
  Baidu,
  Custom,
  ChannelType'
  #-}
