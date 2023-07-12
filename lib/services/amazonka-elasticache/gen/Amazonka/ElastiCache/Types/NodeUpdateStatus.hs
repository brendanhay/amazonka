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
-- Module      : Amazonka.ElastiCache.Types.NodeUpdateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeUpdateStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodeUpdateStatus = NodeUpdateStatus'
  { fromNodeUpdateStatus ::
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
