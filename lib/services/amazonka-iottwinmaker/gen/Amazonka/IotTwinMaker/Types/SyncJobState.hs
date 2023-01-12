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
-- Module      : Amazonka.IotTwinMaker.Types.SyncJobState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.SyncJobState
  ( SyncJobState
      ( ..,
        SyncJobState_ACTIVE,
        SyncJobState_CREATING,
        SyncJobState_DELETING,
        SyncJobState_ERROR,
        SyncJobState_INITIALIZING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SyncJobState = SyncJobState'
  { fromSyncJobState ::
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

pattern SyncJobState_ACTIVE :: SyncJobState
pattern SyncJobState_ACTIVE = SyncJobState' "ACTIVE"

pattern SyncJobState_CREATING :: SyncJobState
pattern SyncJobState_CREATING = SyncJobState' "CREATING"

pattern SyncJobState_DELETING :: SyncJobState
pattern SyncJobState_DELETING = SyncJobState' "DELETING"

pattern SyncJobState_ERROR :: SyncJobState
pattern SyncJobState_ERROR = SyncJobState' "ERROR"

pattern SyncJobState_INITIALIZING :: SyncJobState
pattern SyncJobState_INITIALIZING = SyncJobState' "INITIALIZING"

{-# COMPLETE
  SyncJobState_ACTIVE,
  SyncJobState_CREATING,
  SyncJobState_DELETING,
  SyncJobState_ERROR,
  SyncJobState_INITIALIZING,
  SyncJobState'
  #-}
