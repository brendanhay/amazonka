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
-- Module      : Network.AWS.EC2.Types.InstanceStateName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateName
  ( InstanceStateName
      ( ..,
        InstanceStateName_Pending,
        InstanceStateName_Running,
        InstanceStateName_Shutting_down,
        InstanceStateName_Stopped,
        InstanceStateName_Stopping,
        InstanceStateName_Terminated
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype InstanceStateName = InstanceStateName'
  { fromInstanceStateName ::
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

pattern InstanceStateName_Pending :: InstanceStateName
pattern InstanceStateName_Pending = InstanceStateName' "pending"

pattern InstanceStateName_Running :: InstanceStateName
pattern InstanceStateName_Running = InstanceStateName' "running"

pattern InstanceStateName_Shutting_down :: InstanceStateName
pattern InstanceStateName_Shutting_down = InstanceStateName' "shutting-down"

pattern InstanceStateName_Stopped :: InstanceStateName
pattern InstanceStateName_Stopped = InstanceStateName' "stopped"

pattern InstanceStateName_Stopping :: InstanceStateName
pattern InstanceStateName_Stopping = InstanceStateName' "stopping"

pattern InstanceStateName_Terminated :: InstanceStateName
pattern InstanceStateName_Terminated = InstanceStateName' "terminated"

{-# COMPLETE
  InstanceStateName_Pending,
  InstanceStateName_Running,
  InstanceStateName_Shutting_down,
  InstanceStateName_Stopped,
  InstanceStateName_Stopping,
  InstanceStateName_Terminated,
  InstanceStateName'
  #-}
