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
-- Module      : Amazonka.SMS.Types.ReplicationRunState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.ReplicationRunState
  ( ReplicationRunState
      ( ..,
        ReplicationRunState_ACTIVE,
        ReplicationRunState_COMPLETED,
        ReplicationRunState_DELETED,
        ReplicationRunState_DELETING,
        ReplicationRunState_FAILED,
        ReplicationRunState_MISSED,
        ReplicationRunState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplicationRunState = ReplicationRunState'
  { fromReplicationRunState ::
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

pattern ReplicationRunState_ACTIVE :: ReplicationRunState
pattern ReplicationRunState_ACTIVE = ReplicationRunState' "ACTIVE"

pattern ReplicationRunState_COMPLETED :: ReplicationRunState
pattern ReplicationRunState_COMPLETED = ReplicationRunState' "COMPLETED"

pattern ReplicationRunState_DELETED :: ReplicationRunState
pattern ReplicationRunState_DELETED = ReplicationRunState' "DELETED"

pattern ReplicationRunState_DELETING :: ReplicationRunState
pattern ReplicationRunState_DELETING = ReplicationRunState' "DELETING"

pattern ReplicationRunState_FAILED :: ReplicationRunState
pattern ReplicationRunState_FAILED = ReplicationRunState' "FAILED"

pattern ReplicationRunState_MISSED :: ReplicationRunState
pattern ReplicationRunState_MISSED = ReplicationRunState' "MISSED"

pattern ReplicationRunState_PENDING :: ReplicationRunState
pattern ReplicationRunState_PENDING = ReplicationRunState' "PENDING"

{-# COMPLETE
  ReplicationRunState_ACTIVE,
  ReplicationRunState_COMPLETED,
  ReplicationRunState_DELETED,
  ReplicationRunState_DELETING,
  ReplicationRunState_FAILED,
  ReplicationRunState_MISSED,
  ReplicationRunState_PENDING,
  ReplicationRunState'
  #-}
