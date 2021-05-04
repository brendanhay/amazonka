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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype InstanceStateName = InstanceStateName'
  { fromInstanceStateName ::
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
