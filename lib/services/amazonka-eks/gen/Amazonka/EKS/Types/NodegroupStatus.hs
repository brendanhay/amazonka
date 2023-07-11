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
-- Module      : Amazonka.EKS.Types.NodegroupStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.NodegroupStatus
  ( NodegroupStatus
      ( ..,
        NodegroupStatus_ACTIVE,
        NodegroupStatus_CREATE_FAILED,
        NodegroupStatus_CREATING,
        NodegroupStatus_DEGRADED,
        NodegroupStatus_DELETE_FAILED,
        NodegroupStatus_DELETING,
        NodegroupStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodegroupStatus = NodegroupStatus'
  { fromNodegroupStatus ::
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

pattern NodegroupStatus_ACTIVE :: NodegroupStatus
pattern NodegroupStatus_ACTIVE = NodegroupStatus' "ACTIVE"

pattern NodegroupStatus_CREATE_FAILED :: NodegroupStatus
pattern NodegroupStatus_CREATE_FAILED = NodegroupStatus' "CREATE_FAILED"

pattern NodegroupStatus_CREATING :: NodegroupStatus
pattern NodegroupStatus_CREATING = NodegroupStatus' "CREATING"

pattern NodegroupStatus_DEGRADED :: NodegroupStatus
pattern NodegroupStatus_DEGRADED = NodegroupStatus' "DEGRADED"

pattern NodegroupStatus_DELETE_FAILED :: NodegroupStatus
pattern NodegroupStatus_DELETE_FAILED = NodegroupStatus' "DELETE_FAILED"

pattern NodegroupStatus_DELETING :: NodegroupStatus
pattern NodegroupStatus_DELETING = NodegroupStatus' "DELETING"

pattern NodegroupStatus_UPDATING :: NodegroupStatus
pattern NodegroupStatus_UPDATING = NodegroupStatus' "UPDATING"

{-# COMPLETE
  NodegroupStatus_ACTIVE,
  NodegroupStatus_CREATE_FAILED,
  NodegroupStatus_CREATING,
  NodegroupStatus_DEGRADED,
  NodegroupStatus_DELETE_FAILED,
  NodegroupStatus_DELETING,
  NodegroupStatus_UPDATING,
  NodegroupStatus'
  #-}
