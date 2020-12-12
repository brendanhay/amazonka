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
        CTADM,
        CTAPNS,
        CTAPNSSandbox,
        CTAPNSVoip,
        CTAPNSVoipSandbox,
        CTBaidu,
        CTCustom,
        CTEmail,
        CTGCM,
        CTPush,
        CTSms,
        CTVoice
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

pattern CTADM :: ChannelType
pattern CTADM = ChannelType' "ADM"

pattern CTAPNS :: ChannelType
pattern CTAPNS = ChannelType' "APNS"

pattern CTAPNSSandbox :: ChannelType
pattern CTAPNSSandbox = ChannelType' "APNS_SANDBOX"

pattern CTAPNSVoip :: ChannelType
pattern CTAPNSVoip = ChannelType' "APNS_VOIP"

pattern CTAPNSVoipSandbox :: ChannelType
pattern CTAPNSVoipSandbox = ChannelType' "APNS_VOIP_SANDBOX"

pattern CTBaidu :: ChannelType
pattern CTBaidu = ChannelType' "BAIDU"

pattern CTCustom :: ChannelType
pattern CTCustom = ChannelType' "CUSTOM"

pattern CTEmail :: ChannelType
pattern CTEmail = ChannelType' "EMAIL"

pattern CTGCM :: ChannelType
pattern CTGCM = ChannelType' "GCM"

pattern CTPush :: ChannelType
pattern CTPush = ChannelType' "PUSH"

pattern CTSms :: ChannelType
pattern CTSms = ChannelType' "SMS"

pattern CTVoice :: ChannelType
pattern CTVoice = ChannelType' "VOICE"

{-# COMPLETE
  CTADM,
  CTAPNS,
  CTAPNSSandbox,
  CTAPNSVoip,
  CTAPNSVoipSandbox,
  CTBaidu,
  CTCustom,
  CTEmail,
  CTGCM,
  CTPush,
  CTSms,
  CTVoice,
  ChannelType'
  #-}
