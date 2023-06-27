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
-- Module      : Amazonka.Route53Resolver.Types.ResolverDNSSECValidationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverDNSSECValidationStatus
  ( ResolverDNSSECValidationStatus
      ( ..,
        ResolverDNSSECValidationStatus_DISABLED,
        ResolverDNSSECValidationStatus_DISABLING,
        ResolverDNSSECValidationStatus_ENABLED,
        ResolverDNSSECValidationStatus_ENABLING,
        ResolverDNSSECValidationStatus_UPDATING_TO_USE_LOCAL_RESOURCE_SETTING,
        ResolverDNSSECValidationStatus_USE_LOCAL_RESOURCE_SETTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResolverDNSSECValidationStatus = ResolverDNSSECValidationStatus'
  { fromResolverDNSSECValidationStatus ::
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

pattern ResolverDNSSECValidationStatus_DISABLED :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_DISABLED = ResolverDNSSECValidationStatus' "DISABLED"

pattern ResolverDNSSECValidationStatus_DISABLING :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_DISABLING = ResolverDNSSECValidationStatus' "DISABLING"

pattern ResolverDNSSECValidationStatus_ENABLED :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_ENABLED = ResolverDNSSECValidationStatus' "ENABLED"

pattern ResolverDNSSECValidationStatus_ENABLING :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_ENABLING = ResolverDNSSECValidationStatus' "ENABLING"

pattern ResolverDNSSECValidationStatus_UPDATING_TO_USE_LOCAL_RESOURCE_SETTING :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_UPDATING_TO_USE_LOCAL_RESOURCE_SETTING = ResolverDNSSECValidationStatus' "UPDATING_TO_USE_LOCAL_RESOURCE_SETTING"

pattern ResolverDNSSECValidationStatus_USE_LOCAL_RESOURCE_SETTING :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_USE_LOCAL_RESOURCE_SETTING = ResolverDNSSECValidationStatus' "USE_LOCAL_RESOURCE_SETTING"

{-# COMPLETE
  ResolverDNSSECValidationStatus_DISABLED,
  ResolverDNSSECValidationStatus_DISABLING,
  ResolverDNSSECValidationStatus_ENABLED,
  ResolverDNSSECValidationStatus_ENABLING,
  ResolverDNSSECValidationStatus_UPDATING_TO_USE_LOCAL_RESOURCE_SETTING,
  ResolverDNSSECValidationStatus_USE_LOCAL_RESOURCE_SETTING,
  ResolverDNSSECValidationStatus'
  #-}
