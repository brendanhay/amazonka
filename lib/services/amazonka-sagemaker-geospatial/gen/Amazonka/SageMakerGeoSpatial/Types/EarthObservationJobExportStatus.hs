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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobExportStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobExportStatus
  ( EarthObservationJobExportStatus
      ( ..,
        EarthObservationJobExportStatus_FAILED,
        EarthObservationJobExportStatus_IN_PROGRESS,
        EarthObservationJobExportStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EarthObservationJobExportStatus = EarthObservationJobExportStatus'
  { fromEarthObservationJobExportStatus ::
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

pattern EarthObservationJobExportStatus_FAILED :: EarthObservationJobExportStatus
pattern EarthObservationJobExportStatus_FAILED = EarthObservationJobExportStatus' "FAILED"

pattern EarthObservationJobExportStatus_IN_PROGRESS :: EarthObservationJobExportStatus
pattern EarthObservationJobExportStatus_IN_PROGRESS = EarthObservationJobExportStatus' "IN_PROGRESS"

pattern EarthObservationJobExportStatus_SUCCEEDED :: EarthObservationJobExportStatus
pattern EarthObservationJobExportStatus_SUCCEEDED = EarthObservationJobExportStatus' "SUCCEEDED"

{-# COMPLETE
  EarthObservationJobExportStatus_FAILED,
  EarthObservationJobExportStatus_IN_PROGRESS,
  EarthObservationJobExportStatus_SUCCEEDED,
  EarthObservationJobExportStatus'
  #-}
