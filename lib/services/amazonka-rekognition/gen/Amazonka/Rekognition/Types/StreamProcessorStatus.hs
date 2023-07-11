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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorStatus
  ( StreamProcessorStatus
      ( ..,
        StreamProcessorStatus_FAILED,
        StreamProcessorStatus_RUNNING,
        StreamProcessorStatus_STARTING,
        StreamProcessorStatus_STOPPED,
        StreamProcessorStatus_STOPPING,
        StreamProcessorStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StreamProcessorStatus = StreamProcessorStatus'
  { fromStreamProcessorStatus ::
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

pattern StreamProcessorStatus_FAILED :: StreamProcessorStatus
pattern StreamProcessorStatus_FAILED = StreamProcessorStatus' "FAILED"

pattern StreamProcessorStatus_RUNNING :: StreamProcessorStatus
pattern StreamProcessorStatus_RUNNING = StreamProcessorStatus' "RUNNING"

pattern StreamProcessorStatus_STARTING :: StreamProcessorStatus
pattern StreamProcessorStatus_STARTING = StreamProcessorStatus' "STARTING"

pattern StreamProcessorStatus_STOPPED :: StreamProcessorStatus
pattern StreamProcessorStatus_STOPPED = StreamProcessorStatus' "STOPPED"

pattern StreamProcessorStatus_STOPPING :: StreamProcessorStatus
pattern StreamProcessorStatus_STOPPING = StreamProcessorStatus' "STOPPING"

pattern StreamProcessorStatus_UPDATING :: StreamProcessorStatus
pattern StreamProcessorStatus_UPDATING = StreamProcessorStatus' "UPDATING"

{-# COMPLETE
  StreamProcessorStatus_FAILED,
  StreamProcessorStatus_RUNNING,
  StreamProcessorStatus_STARTING,
  StreamProcessorStatus_STOPPED,
  StreamProcessorStatus_STOPPING,
  StreamProcessorStatus_UPDATING,
  StreamProcessorStatus'
  #-}
