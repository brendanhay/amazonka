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
-- Module      : Amazonka.DrS.Types.ReplicationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationStatus
  ( ReplicationStatus
      ( ..,
        ReplicationStatus_ERROR,
        ReplicationStatus_IN_PROGRESS,
        ReplicationStatus_PROTECTED,
        ReplicationStatus_STOPPED
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

pattern ReplicationStatus_ERROR :: ReplicationStatus
pattern ReplicationStatus_ERROR = ReplicationStatus' "ERROR"

pattern ReplicationStatus_IN_PROGRESS :: ReplicationStatus
pattern ReplicationStatus_IN_PROGRESS = ReplicationStatus' "IN_PROGRESS"

pattern ReplicationStatus_PROTECTED :: ReplicationStatus
pattern ReplicationStatus_PROTECTED = ReplicationStatus' "PROTECTED"

pattern ReplicationStatus_STOPPED :: ReplicationStatus
pattern ReplicationStatus_STOPPED = ReplicationStatus' "STOPPED"

{-# COMPLETE
  ReplicationStatus_ERROR,
  ReplicationStatus_IN_PROGRESS,
  ReplicationStatus_PROTECTED,
  ReplicationStatus_STOPPED,
  ReplicationStatus'
  #-}
