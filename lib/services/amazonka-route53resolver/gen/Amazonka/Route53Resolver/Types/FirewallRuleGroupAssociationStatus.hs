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
-- Module      : Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.FirewallRuleGroupAssociationStatus
  ( FirewallRuleGroupAssociationStatus
      ( ..,
        FirewallRuleGroupAssociationStatus_COMPLETE,
        FirewallRuleGroupAssociationStatus_DELETING,
        FirewallRuleGroupAssociationStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FirewallRuleGroupAssociationStatus = FirewallRuleGroupAssociationStatus'
  { fromFirewallRuleGroupAssociationStatus ::
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

pattern FirewallRuleGroupAssociationStatus_COMPLETE :: FirewallRuleGroupAssociationStatus
pattern FirewallRuleGroupAssociationStatus_COMPLETE = FirewallRuleGroupAssociationStatus' "COMPLETE"

pattern FirewallRuleGroupAssociationStatus_DELETING :: FirewallRuleGroupAssociationStatus
pattern FirewallRuleGroupAssociationStatus_DELETING = FirewallRuleGroupAssociationStatus' "DELETING"

pattern FirewallRuleGroupAssociationStatus_UPDATING :: FirewallRuleGroupAssociationStatus
pattern FirewallRuleGroupAssociationStatus_UPDATING = FirewallRuleGroupAssociationStatus' "UPDATING"

{-# COMPLETE
  FirewallRuleGroupAssociationStatus_COMPLETE,
  FirewallRuleGroupAssociationStatus_DELETING,
  FirewallRuleGroupAssociationStatus_UPDATING,
  FirewallRuleGroupAssociationStatus'
  #-}
