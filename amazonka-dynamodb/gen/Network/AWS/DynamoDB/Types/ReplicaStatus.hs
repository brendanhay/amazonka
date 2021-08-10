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
-- Module      : Network.AWS.DynamoDB.Types.ReplicaStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ReplicaStatus = ReplicaStatus'
  { fromReplicaStatus ::
      Core.Text
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
