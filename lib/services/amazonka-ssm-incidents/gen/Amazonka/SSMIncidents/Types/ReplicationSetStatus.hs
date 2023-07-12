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
-- Module      : Amazonka.SSMIncidents.Types.ReplicationSetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ReplicationSetStatus
  ( ReplicationSetStatus
      ( ..,
        ReplicationSetStatus_ACTIVE,
        ReplicationSetStatus_CREATING,
        ReplicationSetStatus_DELETING,
        ReplicationSetStatus_FAILED,
        ReplicationSetStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplicationSetStatus = ReplicationSetStatus'
  { fromReplicationSetStatus ::
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

pattern ReplicationSetStatus_ACTIVE :: ReplicationSetStatus
pattern ReplicationSetStatus_ACTIVE = ReplicationSetStatus' "ACTIVE"

pattern ReplicationSetStatus_CREATING :: ReplicationSetStatus
pattern ReplicationSetStatus_CREATING = ReplicationSetStatus' "CREATING"

pattern ReplicationSetStatus_DELETING :: ReplicationSetStatus
pattern ReplicationSetStatus_DELETING = ReplicationSetStatus' "DELETING"

pattern ReplicationSetStatus_FAILED :: ReplicationSetStatus
pattern ReplicationSetStatus_FAILED = ReplicationSetStatus' "FAILED"

pattern ReplicationSetStatus_UPDATING :: ReplicationSetStatus
pattern ReplicationSetStatus_UPDATING = ReplicationSetStatus' "UPDATING"

{-# COMPLETE
  ReplicationSetStatus_ACTIVE,
  ReplicationSetStatus_CREATING,
  ReplicationSetStatus_DELETING,
  ReplicationSetStatus_FAILED,
  ReplicationSetStatus_UPDATING,
  ReplicationSetStatus'
  #-}
