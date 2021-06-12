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
-- Module      : Network.AWS.Route53.Types.AccountLimitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AccountLimitType
  ( AccountLimitType
      ( ..,
        AccountLimitType_MAX_HEALTH_CHECKS_BY_OWNER,
        AccountLimitType_MAX_HOSTED_ZONES_BY_OWNER,
        AccountLimitType_MAX_REUSABLE_DELEGATION_SETS_BY_OWNER,
        AccountLimitType_MAX_TRAFFIC_POLICIES_BY_OWNER,
        AccountLimitType_MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Route53.Internal

newtype AccountLimitType = AccountLimitType'
  { fromAccountLimitType ::
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

pattern AccountLimitType_MAX_HEALTH_CHECKS_BY_OWNER :: AccountLimitType
pattern AccountLimitType_MAX_HEALTH_CHECKS_BY_OWNER = AccountLimitType' "MAX_HEALTH_CHECKS_BY_OWNER"

pattern AccountLimitType_MAX_HOSTED_ZONES_BY_OWNER :: AccountLimitType
pattern AccountLimitType_MAX_HOSTED_ZONES_BY_OWNER = AccountLimitType' "MAX_HOSTED_ZONES_BY_OWNER"

pattern AccountLimitType_MAX_REUSABLE_DELEGATION_SETS_BY_OWNER :: AccountLimitType
pattern AccountLimitType_MAX_REUSABLE_DELEGATION_SETS_BY_OWNER = AccountLimitType' "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"

pattern AccountLimitType_MAX_TRAFFIC_POLICIES_BY_OWNER :: AccountLimitType
pattern AccountLimitType_MAX_TRAFFIC_POLICIES_BY_OWNER = AccountLimitType' "MAX_TRAFFIC_POLICIES_BY_OWNER"

pattern AccountLimitType_MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER :: AccountLimitType
pattern AccountLimitType_MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER = AccountLimitType' "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"

{-# COMPLETE
  AccountLimitType_MAX_HEALTH_CHECKS_BY_OWNER,
  AccountLimitType_MAX_HOSTED_ZONES_BY_OWNER,
  AccountLimitType_MAX_REUSABLE_DELEGATION_SETS_BY_OWNER,
  AccountLimitType_MAX_TRAFFIC_POLICIES_BY_OWNER,
  AccountLimitType_MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER,
  AccountLimitType'
  #-}
