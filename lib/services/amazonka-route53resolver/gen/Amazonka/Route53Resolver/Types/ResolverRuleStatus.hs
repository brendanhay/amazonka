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
-- Module      : Amazonka.Route53Resolver.Types.ResolverRuleStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverRuleStatus
  ( ResolverRuleStatus
      ( ..,
        ResolverRuleStatus_COMPLETE,
        ResolverRuleStatus_DELETING,
        ResolverRuleStatus_FAILED,
        ResolverRuleStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResolverRuleStatus = ResolverRuleStatus'
  { fromResolverRuleStatus ::
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

pattern ResolverRuleStatus_COMPLETE :: ResolverRuleStatus
pattern ResolverRuleStatus_COMPLETE = ResolverRuleStatus' "COMPLETE"

pattern ResolverRuleStatus_DELETING :: ResolverRuleStatus
pattern ResolverRuleStatus_DELETING = ResolverRuleStatus' "DELETING"

pattern ResolverRuleStatus_FAILED :: ResolverRuleStatus
pattern ResolverRuleStatus_FAILED = ResolverRuleStatus' "FAILED"

pattern ResolverRuleStatus_UPDATING :: ResolverRuleStatus
pattern ResolverRuleStatus_UPDATING = ResolverRuleStatus' "UPDATING"

{-# COMPLETE
  ResolverRuleStatus_COMPLETE,
  ResolverRuleStatus_DELETING,
  ResolverRuleStatus_FAILED,
  ResolverRuleStatus_UPDATING,
  ResolverRuleStatus'
  #-}
