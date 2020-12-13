{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointTypesElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointTypesElement
  ( EndpointTypesElement
      ( EndpointTypesElement',
        ETEPush,
        ETEGCM,
        ETEAPNS,
        ETEAPNSSandbox,
        ETEAPNSVoip,
        ETEAPNSVoipSandbox,
        ETEADM,
        ETESms,
        ETEVoice,
        ETEEmail,
        ETEBaidu,
        ETECustom
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EndpointTypesElement = EndpointTypesElement' Lude.Text
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

pattern ETEPush :: EndpointTypesElement
pattern ETEPush = EndpointTypesElement' "PUSH"

pattern ETEGCM :: EndpointTypesElement
pattern ETEGCM = EndpointTypesElement' "GCM"

pattern ETEAPNS :: EndpointTypesElement
pattern ETEAPNS = EndpointTypesElement' "APNS"

pattern ETEAPNSSandbox :: EndpointTypesElement
pattern ETEAPNSSandbox = EndpointTypesElement' "APNS_SANDBOX"

pattern ETEAPNSVoip :: EndpointTypesElement
pattern ETEAPNSVoip = EndpointTypesElement' "APNS_VOIP"

pattern ETEAPNSVoipSandbox :: EndpointTypesElement
pattern ETEAPNSVoipSandbox = EndpointTypesElement' "APNS_VOIP_SANDBOX"

pattern ETEADM :: EndpointTypesElement
pattern ETEADM = EndpointTypesElement' "ADM"

pattern ETESms :: EndpointTypesElement
pattern ETESms = EndpointTypesElement' "SMS"

pattern ETEVoice :: EndpointTypesElement
pattern ETEVoice = EndpointTypesElement' "VOICE"

pattern ETEEmail :: EndpointTypesElement
pattern ETEEmail = EndpointTypesElement' "EMAIL"

pattern ETEBaidu :: EndpointTypesElement
pattern ETEBaidu = EndpointTypesElement' "BAIDU"

pattern ETECustom :: EndpointTypesElement
pattern ETECustom = EndpointTypesElement' "CUSTOM"

{-# COMPLETE
  ETEPush,
  ETEGCM,
  ETEAPNS,
  ETEAPNSSandbox,
  ETEAPNSVoip,
  ETEAPNSVoipSandbox,
  ETEADM,
  ETESms,
  ETEVoice,
  ETEEmail,
  ETEBaidu,
  ETECustom,
  EndpointTypesElement'
  #-}
