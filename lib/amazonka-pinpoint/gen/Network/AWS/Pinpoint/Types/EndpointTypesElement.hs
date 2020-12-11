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
        ADM,
        APNS,
        APNSSandbox,
        APNSVoip,
        APNSVoipSandbox,
        Baidu,
        Custom,
        Email,
        GCM,
        Push,
        Sms,
        Voice
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

pattern ADM :: EndpointTypesElement
pattern ADM = EndpointTypesElement' "ADM"

pattern APNS :: EndpointTypesElement
pattern APNS = EndpointTypesElement' "APNS"

pattern APNSSandbox :: EndpointTypesElement
pattern APNSSandbox = EndpointTypesElement' "APNS_SANDBOX"

pattern APNSVoip :: EndpointTypesElement
pattern APNSVoip = EndpointTypesElement' "APNS_VOIP"

pattern APNSVoipSandbox :: EndpointTypesElement
pattern APNSVoipSandbox = EndpointTypesElement' "APNS_VOIP_SANDBOX"

pattern Baidu :: EndpointTypesElement
pattern Baidu = EndpointTypesElement' "BAIDU"

pattern Custom :: EndpointTypesElement
pattern Custom = EndpointTypesElement' "CUSTOM"

pattern Email :: EndpointTypesElement
pattern Email = EndpointTypesElement' "EMAIL"

pattern GCM :: EndpointTypesElement
pattern GCM = EndpointTypesElement' "GCM"

pattern Push :: EndpointTypesElement
pattern Push = EndpointTypesElement' "PUSH"

pattern Sms :: EndpointTypesElement
pattern Sms = EndpointTypesElement' "SMS"

pattern Voice :: EndpointTypesElement
pattern Voice = EndpointTypesElement' "VOICE"

{-# COMPLETE
  ADM,
  APNS,
  APNSSandbox,
  APNSVoip,
  APNSVoipSandbox,
  Baidu,
  Custom,
  Email,
  GCM,
  Push,
  Sms,
  Voice,
  EndpointTypesElement'
  #-}
