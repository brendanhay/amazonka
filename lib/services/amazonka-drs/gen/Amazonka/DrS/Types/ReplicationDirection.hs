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
-- Module      : Amazonka.DrS.Types.ReplicationDirection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationDirection
  ( ReplicationDirection
      ( ..,
        ReplicationDirection_FAILBACK,
        ReplicationDirection_FAILOVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Replication direction designates if this is a failover replication, or a
-- failback replication. When a DRS agent is installed on an instance, the
-- replication direction is failover. In cases where a recovery launch was
-- made in the recovery location and a new recovery instance was created,
-- and then a failback replication was initiated from that recovery
-- instance back to the origin location, then the replication direction
-- will be failback.
newtype ReplicationDirection = ReplicationDirection'
  { fromReplicationDirection ::
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

pattern ReplicationDirection_FAILBACK :: ReplicationDirection
pattern ReplicationDirection_FAILBACK = ReplicationDirection' "FAILBACK"

pattern ReplicationDirection_FAILOVER :: ReplicationDirection
pattern ReplicationDirection_FAILOVER = ReplicationDirection' "FAILOVER"

{-# COMPLETE
  ReplicationDirection_FAILBACK,
  ReplicationDirection_FAILOVER,
  ReplicationDirection'
  #-}
