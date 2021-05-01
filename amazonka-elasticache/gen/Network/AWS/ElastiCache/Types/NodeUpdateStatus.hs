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
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeUpdateStatus
  ( NodeUpdateStatus
      ( ..,
        NodeUpdateStatus_Complete,
        NodeUpdateStatus_In_progress,
        NodeUpdateStatus_Not_applied,
        NodeUpdateStatus_Stopped,
        NodeUpdateStatus_Stopping,
        NodeUpdateStatus_Waiting_to_start
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype NodeUpdateStatus = NodeUpdateStatus'
  { fromNodeUpdateStatus ::
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

pattern NodeUpdateStatus_Complete :: NodeUpdateStatus
pattern NodeUpdateStatus_Complete = NodeUpdateStatus' "complete"

pattern NodeUpdateStatus_In_progress :: NodeUpdateStatus
pattern NodeUpdateStatus_In_progress = NodeUpdateStatus' "in-progress"

pattern NodeUpdateStatus_Not_applied :: NodeUpdateStatus
pattern NodeUpdateStatus_Not_applied = NodeUpdateStatus' "not-applied"

pattern NodeUpdateStatus_Stopped :: NodeUpdateStatus
pattern NodeUpdateStatus_Stopped = NodeUpdateStatus' "stopped"

pattern NodeUpdateStatus_Stopping :: NodeUpdateStatus
pattern NodeUpdateStatus_Stopping = NodeUpdateStatus' "stopping"

pattern NodeUpdateStatus_Waiting_to_start :: NodeUpdateStatus
pattern NodeUpdateStatus_Waiting_to_start = NodeUpdateStatus' "waiting-to-start"

{-# COMPLETE
  NodeUpdateStatus_Complete,
  NodeUpdateStatus_In_progress,
  NodeUpdateStatus_Not_applied,
  NodeUpdateStatus_Stopped,
  NodeUpdateStatus_Stopping,
  NodeUpdateStatus_Waiting_to_start,
  NodeUpdateStatus'
  #-}
