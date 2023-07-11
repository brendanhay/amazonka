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
-- Module      : Amazonka.EC2.Types.InstanceStateName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceStateName
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype InstanceStateName = InstanceStateName'
  { fromInstanceStateName ::
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
