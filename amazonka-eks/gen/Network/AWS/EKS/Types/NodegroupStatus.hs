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
-- Module      : Network.AWS.EKS.Types.NodegroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.NodegroupStatus
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

import qualified Network.AWS.Core as Core

newtype NodegroupStatus = NodegroupStatus'
  { fromNodegroupStatus ::
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
