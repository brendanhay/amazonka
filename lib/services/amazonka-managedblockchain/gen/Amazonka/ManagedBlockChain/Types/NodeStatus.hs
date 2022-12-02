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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeStatus
  ( NodeStatus
      ( ..,
        NodeStatus_AVAILABLE,
        NodeStatus_CREATE_FAILED,
        NodeStatus_CREATING,
        NodeStatus_DELETED,
        NodeStatus_DELETING,
        NodeStatus_FAILED,
        NodeStatus_INACCESSIBLE_ENCRYPTION_KEY,
        NodeStatus_UNHEALTHY,
        NodeStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodeStatus = NodeStatus'
  { fromNodeStatus ::
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

pattern NodeStatus_AVAILABLE :: NodeStatus
pattern NodeStatus_AVAILABLE = NodeStatus' "AVAILABLE"

pattern NodeStatus_CREATE_FAILED :: NodeStatus
pattern NodeStatus_CREATE_FAILED = NodeStatus' "CREATE_FAILED"

pattern NodeStatus_CREATING :: NodeStatus
pattern NodeStatus_CREATING = NodeStatus' "CREATING"

pattern NodeStatus_DELETED :: NodeStatus
pattern NodeStatus_DELETED = NodeStatus' "DELETED"

pattern NodeStatus_DELETING :: NodeStatus
pattern NodeStatus_DELETING = NodeStatus' "DELETING"

pattern NodeStatus_FAILED :: NodeStatus
pattern NodeStatus_FAILED = NodeStatus' "FAILED"

pattern NodeStatus_INACCESSIBLE_ENCRYPTION_KEY :: NodeStatus
pattern NodeStatus_INACCESSIBLE_ENCRYPTION_KEY = NodeStatus' "INACCESSIBLE_ENCRYPTION_KEY"

pattern NodeStatus_UNHEALTHY :: NodeStatus
pattern NodeStatus_UNHEALTHY = NodeStatus' "UNHEALTHY"

pattern NodeStatus_UPDATING :: NodeStatus
pattern NodeStatus_UPDATING = NodeStatus' "UPDATING"

{-# COMPLETE
  NodeStatus_AVAILABLE,
  NodeStatus_CREATE_FAILED,
  NodeStatus_CREATING,
  NodeStatus_DELETED,
  NodeStatus_DELETING,
  NodeStatus_FAILED,
  NodeStatus_INACCESSIBLE_ENCRYPTION_KEY,
  NodeStatus_UNHEALTHY,
  NodeStatus_UPDATING,
  NodeStatus'
  #-}
