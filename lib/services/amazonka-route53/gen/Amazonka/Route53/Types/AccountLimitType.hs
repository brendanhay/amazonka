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
-- Module      : Amazonka.Route53.Types.AccountLimitType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.AccountLimitType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype AccountLimitType = AccountLimitType'
  { fromAccountLimitType ::
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
