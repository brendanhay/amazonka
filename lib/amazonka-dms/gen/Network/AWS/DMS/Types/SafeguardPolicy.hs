{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SafeguardPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SafeguardPolicy
  ( SafeguardPolicy
      ( SafeguardPolicy',
        SafeguardPolicyRelyOnSqlServerReplicationAgent,
        SafeguardPolicyExclusiveAutomaticTruncation,
        SafeguardPolicySharedAutomaticTruncation,
        fromSafeguardPolicy
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SafeguardPolicy = SafeguardPolicy'
  { fromSafeguardPolicy ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SafeguardPolicyRelyOnSqlServerReplicationAgent :: SafeguardPolicy
pattern SafeguardPolicyRelyOnSqlServerReplicationAgent = SafeguardPolicy' "rely-on-sql-server-replication-agent"

pattern SafeguardPolicyExclusiveAutomaticTruncation :: SafeguardPolicy
pattern SafeguardPolicyExclusiveAutomaticTruncation = SafeguardPolicy' "exclusive-automatic-truncation"

pattern SafeguardPolicySharedAutomaticTruncation :: SafeguardPolicy
pattern SafeguardPolicySharedAutomaticTruncation = SafeguardPolicy' "shared-automatic-truncation"

{-# COMPLETE
  SafeguardPolicyRelyOnSqlServerReplicationAgent,
  SafeguardPolicyExclusiveAutomaticTruncation,
  SafeguardPolicySharedAutomaticTruncation,
  SafeguardPolicy'
  #-}
