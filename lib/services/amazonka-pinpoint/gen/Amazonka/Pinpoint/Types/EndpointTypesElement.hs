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
-- Module      : Amazonka.Pinpoint.Types.EndpointTypesElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointTypesElement
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
        EndpointTypesElement_IN_APP,
        EndpointTypesElement_PUSH,
        EndpointTypesElement_SMS,
        EndpointTypesElement_VOICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EndpointTypesElement = EndpointTypesElement'
  { fromEndpointTypesElement ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

pattern EndpointTypesElement_IN_APP :: EndpointTypesElement
pattern EndpointTypesElement_IN_APP = EndpointTypesElement' "IN_APP"

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
  EndpointTypesElement_IN_APP,
  EndpointTypesElement_PUSH,
  EndpointTypesElement_SMS,
  EndpointTypesElement_VOICE,
  EndpointTypesElement'
  #-}
