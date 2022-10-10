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
-- Module      : Amazonka.Route53Resolver.Types.FirewallDomainListStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallDomainListStatus
  ( FirewallDomainListStatus
      ( ..,
        FirewallDomainListStatus_COMPLETE,
        FirewallDomainListStatus_COMPLETE_IMPORT_FAILED,
        FirewallDomainListStatus_DELETING,
        FirewallDomainListStatus_IMPORTING,
        FirewallDomainListStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FirewallDomainListStatus = FirewallDomainListStatus'
  { fromFirewallDomainListStatus ::
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

pattern FirewallDomainListStatus_COMPLETE :: FirewallDomainListStatus
pattern FirewallDomainListStatus_COMPLETE = FirewallDomainListStatus' "COMPLETE"

pattern FirewallDomainListStatus_COMPLETE_IMPORT_FAILED :: FirewallDomainListStatus
pattern FirewallDomainListStatus_COMPLETE_IMPORT_FAILED = FirewallDomainListStatus' "COMPLETE_IMPORT_FAILED"

pattern FirewallDomainListStatus_DELETING :: FirewallDomainListStatus
pattern FirewallDomainListStatus_DELETING = FirewallDomainListStatus' "DELETING"

pattern FirewallDomainListStatus_IMPORTING :: FirewallDomainListStatus
pattern FirewallDomainListStatus_IMPORTING = FirewallDomainListStatus' "IMPORTING"

pattern FirewallDomainListStatus_UPDATING :: FirewallDomainListStatus
pattern FirewallDomainListStatus_UPDATING = FirewallDomainListStatus' "UPDATING"

{-# COMPLETE
  FirewallDomainListStatus_COMPLETE,
  FirewallDomainListStatus_COMPLETE_IMPORT_FAILED,
  FirewallDomainListStatus_DELETING,
  FirewallDomainListStatus_IMPORTING,
  FirewallDomainListStatus_UPDATING,
  FirewallDomainListStatus'
  #-}
