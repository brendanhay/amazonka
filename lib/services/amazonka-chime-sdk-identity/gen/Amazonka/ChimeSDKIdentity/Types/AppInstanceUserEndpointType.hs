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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype AppInstanceUserEndpointType = AppInstanceUserEndpointType'
  { fromAppInstanceUserEndpointType ::
      Core.Text
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
