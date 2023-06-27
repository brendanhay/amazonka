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
-- Module      : Amazonka.EFS.Types.ReplicationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.ReplicationStatus
  ( ReplicationStatus
      ( ..,
        ReplicationStatus_DELETING,
        ReplicationStatus_ENABLED,
        ReplicationStatus_ENABLING,
        ReplicationStatus_ERROR,
        ReplicationStatus_PAUSED,
        ReplicationStatus_PAUSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplicationStatus = ReplicationStatus'
  { fromReplicationStatus ::
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

pattern ReplicationStatus_DELETING :: ReplicationStatus
pattern ReplicationStatus_DELETING = ReplicationStatus' "DELETING"

pattern ReplicationStatus_ENABLED :: ReplicationStatus
pattern ReplicationStatus_ENABLED = ReplicationStatus' "ENABLED"

pattern ReplicationStatus_ENABLING :: ReplicationStatus
pattern ReplicationStatus_ENABLING = ReplicationStatus' "ENABLING"

pattern ReplicationStatus_ERROR :: ReplicationStatus
pattern ReplicationStatus_ERROR = ReplicationStatus' "ERROR"

pattern ReplicationStatus_PAUSED :: ReplicationStatus
pattern ReplicationStatus_PAUSED = ReplicationStatus' "PAUSED"

pattern ReplicationStatus_PAUSING :: ReplicationStatus
pattern ReplicationStatus_PAUSING = ReplicationStatus' "PAUSING"

{-# COMPLETE
  ReplicationStatus_DELETING,
  ReplicationStatus_ENABLED,
  ReplicationStatus_ENABLING,
  ReplicationStatus_ERROR,
  ReplicationStatus_PAUSED,
  ReplicationStatus_PAUSING,
  ReplicationStatus'
  #-}
