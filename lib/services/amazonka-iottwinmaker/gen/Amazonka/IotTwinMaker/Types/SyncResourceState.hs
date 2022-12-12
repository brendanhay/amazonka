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
-- Module      : Amazonka.IotTwinMaker.Types.SyncResourceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncResourceState
  ( SyncResourceState
      ( ..,
        SyncResourceState_DELETED,
        SyncResourceState_ERROR,
        SyncResourceState_INITIALIZING,
        SyncResourceState_IN_SYNC,
        SyncResourceState_PROCESSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SyncResourceState = SyncResourceState'
  { fromSyncResourceState ::
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

pattern SyncResourceState_DELETED :: SyncResourceState
pattern SyncResourceState_DELETED = SyncResourceState' "DELETED"

pattern SyncResourceState_ERROR :: SyncResourceState
pattern SyncResourceState_ERROR = SyncResourceState' "ERROR"

pattern SyncResourceState_INITIALIZING :: SyncResourceState
pattern SyncResourceState_INITIALIZING = SyncResourceState' "INITIALIZING"

pattern SyncResourceState_IN_SYNC :: SyncResourceState
pattern SyncResourceState_IN_SYNC = SyncResourceState' "IN_SYNC"

pattern SyncResourceState_PROCESSING :: SyncResourceState
pattern SyncResourceState_PROCESSING = SyncResourceState' "PROCESSING"

{-# COMPLETE
  SyncResourceState_DELETED,
  SyncResourceState_ERROR,
  SyncResourceState_INITIALIZING,
  SyncResourceState_IN_SYNC,
  SyncResourceState_PROCESSING,
  SyncResourceState'
  #-}
