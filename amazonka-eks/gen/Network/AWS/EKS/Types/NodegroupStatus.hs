{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype NodegroupStatus = NodegroupStatus'
  { fromNodegroupStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
