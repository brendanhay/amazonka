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
-- Module      : Amazonka.IoTEvents.Types.DetectorModelVersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorModelVersionStatus
  ( DetectorModelVersionStatus
      ( ..,
        DetectorModelVersionStatus_ACTIVATING,
        DetectorModelVersionStatus_ACTIVE,
        DetectorModelVersionStatus_DEPRECATED,
        DetectorModelVersionStatus_DRAFT,
        DetectorModelVersionStatus_FAILED,
        DetectorModelVersionStatus_INACTIVE,
        DetectorModelVersionStatus_PAUSED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetectorModelVersionStatus = DetectorModelVersionStatus'
  { fromDetectorModelVersionStatus ::
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

pattern DetectorModelVersionStatus_ACTIVATING :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_ACTIVATING = DetectorModelVersionStatus' "ACTIVATING"

pattern DetectorModelVersionStatus_ACTIVE :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_ACTIVE = DetectorModelVersionStatus' "ACTIVE"

pattern DetectorModelVersionStatus_DEPRECATED :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_DEPRECATED = DetectorModelVersionStatus' "DEPRECATED"

pattern DetectorModelVersionStatus_DRAFT :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_DRAFT = DetectorModelVersionStatus' "DRAFT"

pattern DetectorModelVersionStatus_FAILED :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_FAILED = DetectorModelVersionStatus' "FAILED"

pattern DetectorModelVersionStatus_INACTIVE :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_INACTIVE = DetectorModelVersionStatus' "INACTIVE"

pattern DetectorModelVersionStatus_PAUSED :: DetectorModelVersionStatus
pattern DetectorModelVersionStatus_PAUSED = DetectorModelVersionStatus' "PAUSED"

{-# COMPLETE
  DetectorModelVersionStatus_ACTIVATING,
  DetectorModelVersionStatus_ACTIVE,
  DetectorModelVersionStatus_DEPRECATED,
  DetectorModelVersionStatus_DRAFT,
  DetectorModelVersionStatus_FAILED,
  DetectorModelVersionStatus_INACTIVE,
  DetectorModelVersionStatus_PAUSED,
  DetectorModelVersionStatus'
  #-}
