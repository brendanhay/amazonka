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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointType
  ( AppInstanceUserEndpointType
      ( ..,
        AppInstanceUserEndpointType_APNS,
        AppInstanceUserEndpointType_APNS_SANDBOX,
        AppInstanceUserEndpointType_GCM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppInstanceUserEndpointType = AppInstanceUserEndpointType'
  { fromAppInstanceUserEndpointType ::
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

pattern AppInstanceUserEndpointType_APNS :: AppInstanceUserEndpointType
pattern AppInstanceUserEndpointType_APNS = AppInstanceUserEndpointType' "APNS"

pattern AppInstanceUserEndpointType_APNS_SANDBOX :: AppInstanceUserEndpointType
pattern AppInstanceUserEndpointType_APNS_SANDBOX = AppInstanceUserEndpointType' "APNS_SANDBOX"

pattern AppInstanceUserEndpointType_GCM :: AppInstanceUserEndpointType
pattern AppInstanceUserEndpointType_GCM = AppInstanceUserEndpointType' "GCM"

{-# COMPLETE
  AppInstanceUserEndpointType_APNS,
  AppInstanceUserEndpointType_APNS_SANDBOX,
  AppInstanceUserEndpointType_GCM,
  AppInstanceUserEndpointType'
  #-}
