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
-- Module      : Amazonka.OsIs.Types.PipelineStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.PipelineStatus
  ( PipelineStatus
      ( ..,
        PipelineStatus_ACTIVE,
        PipelineStatus_CREATE_FAILED,
        PipelineStatus_CREATING,
        PipelineStatus_DELETING,
        PipelineStatus_STARTING,
        PipelineStatus_START_FAILED,
        PipelineStatus_STOPPED,
        PipelineStatus_STOPPING,
        PipelineStatus_UPDATE_FAILED,
        PipelineStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PipelineStatus = PipelineStatus'
  { fromPipelineStatus ::
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

pattern PipelineStatus_ACTIVE :: PipelineStatus
pattern PipelineStatus_ACTIVE = PipelineStatus' "ACTIVE"

pattern PipelineStatus_CREATE_FAILED :: PipelineStatus
pattern PipelineStatus_CREATE_FAILED = PipelineStatus' "CREATE_FAILED"

pattern PipelineStatus_CREATING :: PipelineStatus
pattern PipelineStatus_CREATING = PipelineStatus' "CREATING"

pattern PipelineStatus_DELETING :: PipelineStatus
pattern PipelineStatus_DELETING = PipelineStatus' "DELETING"

pattern PipelineStatus_STARTING :: PipelineStatus
pattern PipelineStatus_STARTING = PipelineStatus' "STARTING"

pattern PipelineStatus_START_FAILED :: PipelineStatus
pattern PipelineStatus_START_FAILED = PipelineStatus' "START_FAILED"

pattern PipelineStatus_STOPPED :: PipelineStatus
pattern PipelineStatus_STOPPED = PipelineStatus' "STOPPED"

pattern PipelineStatus_STOPPING :: PipelineStatus
pattern PipelineStatus_STOPPING = PipelineStatus' "STOPPING"

pattern PipelineStatus_UPDATE_FAILED :: PipelineStatus
pattern PipelineStatus_UPDATE_FAILED = PipelineStatus' "UPDATE_FAILED"

pattern PipelineStatus_UPDATING :: PipelineStatus
pattern PipelineStatus_UPDATING = PipelineStatus' "UPDATING"

{-# COMPLETE
  PipelineStatus_ACTIVE,
  PipelineStatus_CREATE_FAILED,
  PipelineStatus_CREATING,
  PipelineStatus_DELETING,
  PipelineStatus_STARTING,
  PipelineStatus_START_FAILED,
  PipelineStatus_STOPPED,
  PipelineStatus_STOPPING,
  PipelineStatus_UPDATE_FAILED,
  PipelineStatus_UPDATING,
  PipelineStatus'
  #-}
