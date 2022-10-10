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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
        ResolverDNSSECValidationStatus_ENABLING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ResolverDNSSECValidationStatus = ResolverDNSSECValidationStatus'
  { fromResolverDNSSECValidationStatus ::
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

pattern ResolverDNSSECValidationStatus_DISABLED :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_DISABLED = ResolverDNSSECValidationStatus' "DISABLED"

pattern ResolverDNSSECValidationStatus_DISABLING :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_DISABLING = ResolverDNSSECValidationStatus' "DISABLING"

pattern ResolverDNSSECValidationStatus_ENABLED :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_ENABLED = ResolverDNSSECValidationStatus' "ENABLED"

pattern ResolverDNSSECValidationStatus_ENABLING :: ResolverDNSSECValidationStatus
pattern ResolverDNSSECValidationStatus_ENABLING = ResolverDNSSECValidationStatus' "ENABLING"

{-# COMPLETE
  ResolverDNSSECValidationStatus_DISABLED,
  ResolverDNSSECValidationStatus_DISABLING,
  ResolverDNSSECValidationStatus_ENABLED,
  ResolverDNSSECValidationStatus_ENABLING,
  ResolverDNSSECValidationStatus'
  #-}
