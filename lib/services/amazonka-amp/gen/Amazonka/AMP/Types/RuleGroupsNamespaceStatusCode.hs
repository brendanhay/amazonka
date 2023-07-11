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
-- Module      : Amazonka.AMP.Types.RuleGroupsNamespaceStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.RuleGroupsNamespaceStatusCode
  ( RuleGroupsNamespaceStatusCode
      ( ..,
        RuleGroupsNamespaceStatusCode_ACTIVE,
        RuleGroupsNamespaceStatusCode_CREATING,
        RuleGroupsNamespaceStatusCode_CREATION_FAILED,
        RuleGroupsNamespaceStatusCode_DELETING,
        RuleGroupsNamespaceStatusCode_UPDATE_FAILED,
        RuleGroupsNamespaceStatusCode_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | State of a namespace.
newtype RuleGroupsNamespaceStatusCode = RuleGroupsNamespaceStatusCode'
  { fromRuleGroupsNamespaceStatusCode ::
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

pattern RuleGroupsNamespaceStatusCode_ACTIVE :: RuleGroupsNamespaceStatusCode
pattern RuleGroupsNamespaceStatusCode_ACTIVE = RuleGroupsNamespaceStatusCode' "ACTIVE"

pattern RuleGroupsNamespaceStatusCode_CREATING :: RuleGroupsNamespaceStatusCode
pattern RuleGroupsNamespaceStatusCode_CREATING = RuleGroupsNamespaceStatusCode' "CREATING"

pattern RuleGroupsNamespaceStatusCode_CREATION_FAILED :: RuleGroupsNamespaceStatusCode
pattern RuleGroupsNamespaceStatusCode_CREATION_FAILED = RuleGroupsNamespaceStatusCode' "CREATION_FAILED"

pattern RuleGroupsNamespaceStatusCode_DELETING :: RuleGroupsNamespaceStatusCode
pattern RuleGroupsNamespaceStatusCode_DELETING = RuleGroupsNamespaceStatusCode' "DELETING"

pattern RuleGroupsNamespaceStatusCode_UPDATE_FAILED :: RuleGroupsNamespaceStatusCode
pattern RuleGroupsNamespaceStatusCode_UPDATE_FAILED = RuleGroupsNamespaceStatusCode' "UPDATE_FAILED"

pattern RuleGroupsNamespaceStatusCode_UPDATING :: RuleGroupsNamespaceStatusCode
pattern RuleGroupsNamespaceStatusCode_UPDATING = RuleGroupsNamespaceStatusCode' "UPDATING"

{-# COMPLETE
  RuleGroupsNamespaceStatusCode_ACTIVE,
  RuleGroupsNamespaceStatusCode_CREATING,
  RuleGroupsNamespaceStatusCode_CREATION_FAILED,
  RuleGroupsNamespaceStatusCode_DELETING,
  RuleGroupsNamespaceStatusCode_UPDATE_FAILED,
  RuleGroupsNamespaceStatusCode_UPDATING,
  RuleGroupsNamespaceStatusCode'
  #-}
