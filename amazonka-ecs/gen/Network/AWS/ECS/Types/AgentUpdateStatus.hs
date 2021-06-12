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
-- Module      : Network.AWS.ECS.Types.AgentUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AgentUpdateStatus
  ( AgentUpdateStatus
      ( ..,
        AgentUpdateStatus_FAILED,
        AgentUpdateStatus_PENDING,
        AgentUpdateStatus_STAGED,
        AgentUpdateStatus_STAGING,
        AgentUpdateStatus_UPDATED,
        AgentUpdateStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AgentUpdateStatus = AgentUpdateStatus'
  { fromAgentUpdateStatus ::
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

pattern AgentUpdateStatus_FAILED :: AgentUpdateStatus
pattern AgentUpdateStatus_FAILED = AgentUpdateStatus' "FAILED"

pattern AgentUpdateStatus_PENDING :: AgentUpdateStatus
pattern AgentUpdateStatus_PENDING = AgentUpdateStatus' "PENDING"

pattern AgentUpdateStatus_STAGED :: AgentUpdateStatus
pattern AgentUpdateStatus_STAGED = AgentUpdateStatus' "STAGED"

pattern AgentUpdateStatus_STAGING :: AgentUpdateStatus
pattern AgentUpdateStatus_STAGING = AgentUpdateStatus' "STAGING"

pattern AgentUpdateStatus_UPDATED :: AgentUpdateStatus
pattern AgentUpdateStatus_UPDATED = AgentUpdateStatus' "UPDATED"

pattern AgentUpdateStatus_UPDATING :: AgentUpdateStatus
pattern AgentUpdateStatus_UPDATING = AgentUpdateStatus' "UPDATING"

{-# COMPLETE
  AgentUpdateStatus_FAILED,
  AgentUpdateStatus_PENDING,
  AgentUpdateStatus_STAGED,
  AgentUpdateStatus_STAGING,
  AgentUpdateStatus_UPDATED,
  AgentUpdateStatus_UPDATING,
  AgentUpdateStatus'
  #-}
