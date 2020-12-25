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
        EndpointTypesElementPush,
        EndpointTypesElementGcm,
        EndpointTypesElementApns,
        EndpointTypesElementApnsSandbox,
        EndpointTypesElementApnsVoip,
        EndpointTypesElementApnsVoipSandbox,
        EndpointTypesElementAdm,
        EndpointTypesElementSms,
        EndpointTypesElementVoice,
        EndpointTypesElementEmail,
        EndpointTypesElementBaidu,
        EndpointTypesElementCustom,
        fromEndpointTypesElement
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EndpointTypesElement = EndpointTypesElement'
  { fromEndpointTypesElement ::
      Core.Text
  }
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

pattern EndpointTypesElementPush :: EndpointTypesElement
pattern EndpointTypesElementPush = EndpointTypesElement' "PUSH"

pattern EndpointTypesElementGcm :: EndpointTypesElement
pattern EndpointTypesElementGcm = EndpointTypesElement' "GCM"

pattern EndpointTypesElementApns :: EndpointTypesElement
pattern EndpointTypesElementApns = EndpointTypesElement' "APNS"

pattern EndpointTypesElementApnsSandbox :: EndpointTypesElement
pattern EndpointTypesElementApnsSandbox = EndpointTypesElement' "APNS_SANDBOX"

pattern EndpointTypesElementApnsVoip :: EndpointTypesElement
pattern EndpointTypesElementApnsVoip = EndpointTypesElement' "APNS_VOIP"

pattern EndpointTypesElementApnsVoipSandbox :: EndpointTypesElement
pattern EndpointTypesElementApnsVoipSandbox = EndpointTypesElement' "APNS_VOIP_SANDBOX"

pattern EndpointTypesElementAdm :: EndpointTypesElement
pattern EndpointTypesElementAdm = EndpointTypesElement' "ADM"

pattern EndpointTypesElementSms :: EndpointTypesElement
pattern EndpointTypesElementSms = EndpointTypesElement' "SMS"

pattern EndpointTypesElementVoice :: EndpointTypesElement
pattern EndpointTypesElementVoice = EndpointTypesElement' "VOICE"

pattern EndpointTypesElementEmail :: EndpointTypesElement
pattern EndpointTypesElementEmail = EndpointTypesElement' "EMAIL"

pattern EndpointTypesElementBaidu :: EndpointTypesElement
pattern EndpointTypesElementBaidu = EndpointTypesElement' "BAIDU"

pattern EndpointTypesElementCustom :: EndpointTypesElement
pattern EndpointTypesElementCustom = EndpointTypesElement' "CUSTOM"

{-# COMPLETE
  EndpointTypesElementPush,
  EndpointTypesElementGcm,
  EndpointTypesElementApns,
  EndpointTypesElementApnsSandbox,
  EndpointTypesElementApnsVoip,
  EndpointTypesElementApnsVoipSandbox,
  EndpointTypesElementAdm,
  EndpointTypesElementSms,
  EndpointTypesElementVoice,
  EndpointTypesElementEmail,
  EndpointTypesElementBaidu,
  EndpointTypesElementCustom,
  EndpointTypesElement'
  #-}
