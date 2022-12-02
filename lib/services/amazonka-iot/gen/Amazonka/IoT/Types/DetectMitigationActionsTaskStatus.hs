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
-- Module      : Amazonka.IoT.Types.DetectMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DetectMitigationActionsTaskStatus
  ( DetectMitigationActionsTaskStatus
      ( ..,
        DetectMitigationActionsTaskStatus_CANCELED,
        DetectMitigationActionsTaskStatus_FAILED,
        DetectMitigationActionsTaskStatus_IN_PROGRESS,
        DetectMitigationActionsTaskStatus_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetectMitigationActionsTaskStatus = DetectMitigationActionsTaskStatus'
  { fromDetectMitigationActionsTaskStatus ::
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

pattern DetectMitigationActionsTaskStatus_CANCELED :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_CANCELED = DetectMitigationActionsTaskStatus' "CANCELED"

pattern DetectMitigationActionsTaskStatus_FAILED :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_FAILED = DetectMitigationActionsTaskStatus' "FAILED"

pattern DetectMitigationActionsTaskStatus_IN_PROGRESS :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_IN_PROGRESS = DetectMitigationActionsTaskStatus' "IN_PROGRESS"

pattern DetectMitigationActionsTaskStatus_SUCCESSFUL :: DetectMitigationActionsTaskStatus
pattern DetectMitigationActionsTaskStatus_SUCCESSFUL = DetectMitigationActionsTaskStatus' "SUCCESSFUL"

{-# COMPLETE
  DetectMitigationActionsTaskStatus_CANCELED,
  DetectMitigationActionsTaskStatus_FAILED,
  DetectMitigationActionsTaskStatus_IN_PROGRESS,
  DetectMitigationActionsTaskStatus_SUCCESSFUL,
  DetectMitigationActionsTaskStatus'
  #-}
