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
-- Module      : Amazonka.DynamoDB.Types.ReplicaStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaStatus
  ( ReplicaStatus
      ( ..,
        ReplicaStatus_ACTIVE,
        ReplicaStatus_CREATING,
        ReplicaStatus_CREATION_FAILED,
        ReplicaStatus_DELETING,
        ReplicaStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS,
        ReplicaStatus_REGION_DISABLED,
        ReplicaStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype ReplicaStatus = ReplicaStatus'
  { fromReplicaStatus ::
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

pattern ReplicaStatus_ACTIVE :: ReplicaStatus
pattern ReplicaStatus_ACTIVE = ReplicaStatus' "ACTIVE"

pattern ReplicaStatus_CREATING :: ReplicaStatus
pattern ReplicaStatus_CREATING = ReplicaStatus' "CREATING"

pattern ReplicaStatus_CREATION_FAILED :: ReplicaStatus
pattern ReplicaStatus_CREATION_FAILED = ReplicaStatus' "CREATION_FAILED"

pattern ReplicaStatus_DELETING :: ReplicaStatus
pattern ReplicaStatus_DELETING = ReplicaStatus' "DELETING"

pattern ReplicaStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS :: ReplicaStatus
pattern ReplicaStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS = ReplicaStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

pattern ReplicaStatus_REGION_DISABLED :: ReplicaStatus
pattern ReplicaStatus_REGION_DISABLED = ReplicaStatus' "REGION_DISABLED"

pattern ReplicaStatus_UPDATING :: ReplicaStatus
pattern ReplicaStatus_UPDATING = ReplicaStatus' "UPDATING"

{-# COMPLETE
  ReplicaStatus_ACTIVE,
  ReplicaStatus_CREATING,
  ReplicaStatus_CREATION_FAILED,
  ReplicaStatus_DELETING,
  ReplicaStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS,
  ReplicaStatus_REGION_DISABLED,
  ReplicaStatus_UPDATING,
  ReplicaStatus'
  #-}
