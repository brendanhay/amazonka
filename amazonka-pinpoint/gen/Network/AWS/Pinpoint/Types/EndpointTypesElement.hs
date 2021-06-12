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
-- Module      : Network.AWS.Pinpoint.Types.EndpointTypesElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointTypesElement
  ( EndpointTypesElement
      ( ..,
        EndpointTypesElement_ADM,
        EndpointTypesElement_APNS,
        EndpointTypesElement_APNS_SANDBOX,
        EndpointTypesElement_APNS_VOIP,
        EndpointTypesElement_APNS_VOIP_SANDBOX,
        EndpointTypesElement_BAIDU,
        EndpointTypesElement_CUSTOM,
        EndpointTypesElement_EMAIL,
        EndpointTypesElement_GCM,
        EndpointTypesElement_PUSH,
        EndpointTypesElement_SMS,
        EndpointTypesElement_VOICE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EndpointTypesElement = EndpointTypesElement'
  { fromEndpointTypesElement ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern EndpointTypesElement_ADM :: EndpointTypesElement
pattern EndpointTypesElement_ADM = EndpointTypesElement' "ADM"

pattern EndpointTypesElement_APNS :: EndpointTypesElement
pattern EndpointTypesElement_APNS = EndpointTypesElement' "APNS"

pattern EndpointTypesElement_APNS_SANDBOX :: EndpointTypesElement
pattern EndpointTypesElement_APNS_SANDBOX = EndpointTypesElement' "APNS_SANDBOX"

pattern EndpointTypesElement_APNS_VOIP :: EndpointTypesElement
pattern EndpointTypesElement_APNS_VOIP = EndpointTypesElement' "APNS_VOIP"

pattern EndpointTypesElement_APNS_VOIP_SANDBOX :: EndpointTypesElement
pattern EndpointTypesElement_APNS_VOIP_SANDBOX = EndpointTypesElement' "APNS_VOIP_SANDBOX"

pattern EndpointTypesElement_BAIDU :: EndpointTypesElement
pattern EndpointTypesElement_BAIDU = EndpointTypesElement' "BAIDU"

pattern EndpointTypesElement_CUSTOM :: EndpointTypesElement
pattern EndpointTypesElement_CUSTOM = EndpointTypesElement' "CUSTOM"

pattern EndpointTypesElement_EMAIL :: EndpointTypesElement
pattern EndpointTypesElement_EMAIL = EndpointTypesElement' "EMAIL"

pattern EndpointTypesElement_GCM :: EndpointTypesElement
pattern EndpointTypesElement_GCM = EndpointTypesElement' "GCM"

pattern EndpointTypesElement_PUSH :: EndpointTypesElement
pattern EndpointTypesElement_PUSH = EndpointTypesElement' "PUSH"

pattern EndpointTypesElement_SMS :: EndpointTypesElement
pattern EndpointTypesElement_SMS = EndpointTypesElement' "SMS"

pattern EndpointTypesElement_VOICE :: EndpointTypesElement
pattern EndpointTypesElement_VOICE = EndpointTypesElement' "VOICE"

{-# COMPLETE
  EndpointTypesElement_ADM,
  EndpointTypesElement_APNS,
  EndpointTypesElement_APNS_SANDBOX,
  EndpointTypesElement_APNS_VOIP,
  EndpointTypesElement_APNS_VOIP_SANDBOX,
  EndpointTypesElement_BAIDU,
  EndpointTypesElement_CUSTOM,
  EndpointTypesElement_EMAIL,
  EndpointTypesElement_GCM,
  EndpointTypesElement_PUSH,
  EndpointTypesElement_SMS,
  EndpointTypesElement_VOICE,
  EndpointTypesElement'
  #-}
