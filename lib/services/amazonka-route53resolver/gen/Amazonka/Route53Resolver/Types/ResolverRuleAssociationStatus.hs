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
-- Module      : Amazonka.Route53Resolver.Types.ResolverRuleAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverRuleAssociationStatus
  ( ResolverRuleAssociationStatus
      ( ..,
        ResolverRuleAssociationStatus_COMPLETE,
        ResolverRuleAssociationStatus_CREATING,
        ResolverRuleAssociationStatus_DELETING,
        ResolverRuleAssociationStatus_FAILED,
        ResolverRuleAssociationStatus_OVERRIDDEN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResolverRuleAssociationStatus = ResolverRuleAssociationStatus'
  { fromResolverRuleAssociationStatus ::
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

pattern ResolverRuleAssociationStatus_COMPLETE :: ResolverRuleAssociationStatus
pattern ResolverRuleAssociationStatus_COMPLETE = ResolverRuleAssociationStatus' "COMPLETE"

pattern ResolverRuleAssociationStatus_CREATING :: ResolverRuleAssociationStatus
pattern ResolverRuleAssociationStatus_CREATING = ResolverRuleAssociationStatus' "CREATING"

pattern ResolverRuleAssociationStatus_DELETING :: ResolverRuleAssociationStatus
pattern ResolverRuleAssociationStatus_DELETING = ResolverRuleAssociationStatus' "DELETING"

pattern ResolverRuleAssociationStatus_FAILED :: ResolverRuleAssociationStatus
pattern ResolverRuleAssociationStatus_FAILED = ResolverRuleAssociationStatus' "FAILED"

pattern ResolverRuleAssociationStatus_OVERRIDDEN :: ResolverRuleAssociationStatus
pattern ResolverRuleAssociationStatus_OVERRIDDEN = ResolverRuleAssociationStatus' "OVERRIDDEN"

{-# COMPLETE
  ResolverRuleAssociationStatus_COMPLETE,
  ResolverRuleAssociationStatus_CREATING,
  ResolverRuleAssociationStatus_DELETING,
  ResolverRuleAssociationStatus_FAILED,
  ResolverRuleAssociationStatus_OVERRIDDEN,
  ResolverRuleAssociationStatus'
  #-}
