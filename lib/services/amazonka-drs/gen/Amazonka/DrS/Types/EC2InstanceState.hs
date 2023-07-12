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
-- Module      : Amazonka.DrS.Types.EC2InstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.EC2InstanceState
  ( EC2InstanceState
      ( ..,
        EC2InstanceState_NOT_FOUND,
        EC2InstanceState_PENDING,
        EC2InstanceState_RUNNING,
        EC2InstanceState_SHUTTING_DOWN,
        EC2InstanceState_STOPPED,
        EC2InstanceState_STOPPING,
        EC2InstanceState_TERMINATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EC2InstanceState = EC2InstanceState'
  { fromEC2InstanceState ::
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

pattern EC2InstanceState_NOT_FOUND :: EC2InstanceState
pattern EC2InstanceState_NOT_FOUND = EC2InstanceState' "NOT_FOUND"

pattern EC2InstanceState_PENDING :: EC2InstanceState
pattern EC2InstanceState_PENDING = EC2InstanceState' "PENDING"

pattern EC2InstanceState_RUNNING :: EC2InstanceState
pattern EC2InstanceState_RUNNING = EC2InstanceState' "RUNNING"

pattern EC2InstanceState_SHUTTING_DOWN :: EC2InstanceState
pattern EC2InstanceState_SHUTTING_DOWN = EC2InstanceState' "SHUTTING-DOWN"

pattern EC2InstanceState_STOPPED :: EC2InstanceState
pattern EC2InstanceState_STOPPED = EC2InstanceState' "STOPPED"

pattern EC2InstanceState_STOPPING :: EC2InstanceState
pattern EC2InstanceState_STOPPING = EC2InstanceState' "STOPPING"

pattern EC2InstanceState_TERMINATED :: EC2InstanceState
pattern EC2InstanceState_TERMINATED = EC2InstanceState' "TERMINATED"

{-# COMPLETE
  EC2InstanceState_NOT_FOUND,
  EC2InstanceState_PENDING,
  EC2InstanceState_RUNNING,
  EC2InstanceState_SHUTTING_DOWN,
  EC2InstanceState_STOPPED,
  EC2InstanceState_STOPPING,
  EC2InstanceState_TERMINATED,
  EC2InstanceState'
  #-}
