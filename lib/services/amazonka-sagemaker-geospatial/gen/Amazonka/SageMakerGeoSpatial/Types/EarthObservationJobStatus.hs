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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobStatus
  ( EarthObservationJobStatus
      ( ..,
        EarthObservationJobStatus_COMPLETED,
        EarthObservationJobStatus_DELETED,
        EarthObservationJobStatus_DELETING,
        EarthObservationJobStatus_FAILED,
        EarthObservationJobStatus_INITIALIZING,
        EarthObservationJobStatus_IN_PROGRESS,
        EarthObservationJobStatus_STOPPED,
        EarthObservationJobStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EarthObservationJobStatus = EarthObservationJobStatus'
  { fromEarthObservationJobStatus ::
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

pattern EarthObservationJobStatus_COMPLETED :: EarthObservationJobStatus
pattern EarthObservationJobStatus_COMPLETED = EarthObservationJobStatus' "COMPLETED"

pattern EarthObservationJobStatus_DELETED :: EarthObservationJobStatus
pattern EarthObservationJobStatus_DELETED = EarthObservationJobStatus' "DELETED"

pattern EarthObservationJobStatus_DELETING :: EarthObservationJobStatus
pattern EarthObservationJobStatus_DELETING = EarthObservationJobStatus' "DELETING"

pattern EarthObservationJobStatus_FAILED :: EarthObservationJobStatus
pattern EarthObservationJobStatus_FAILED = EarthObservationJobStatus' "FAILED"

pattern EarthObservationJobStatus_INITIALIZING :: EarthObservationJobStatus
pattern EarthObservationJobStatus_INITIALIZING = EarthObservationJobStatus' "INITIALIZING"

pattern EarthObservationJobStatus_IN_PROGRESS :: EarthObservationJobStatus
pattern EarthObservationJobStatus_IN_PROGRESS = EarthObservationJobStatus' "IN_PROGRESS"

pattern EarthObservationJobStatus_STOPPED :: EarthObservationJobStatus
pattern EarthObservationJobStatus_STOPPED = EarthObservationJobStatus' "STOPPED"

pattern EarthObservationJobStatus_STOPPING :: EarthObservationJobStatus
pattern EarthObservationJobStatus_STOPPING = EarthObservationJobStatus' "STOPPING"

{-# COMPLETE
  EarthObservationJobStatus_COMPLETED,
  EarthObservationJobStatus_DELETED,
  EarthObservationJobStatus_DELETING,
  EarthObservationJobStatus_FAILED,
  EarthObservationJobStatus_INITIALIZING,
  EarthObservationJobStatus_IN_PROGRESS,
  EarthObservationJobStatus_STOPPED,
  EarthObservationJobStatus_STOPPING,
  EarthObservationJobStatus'
  #-}
