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
-- Module      : Amazonka.RobOMaker.Types.WorldExportJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldExportJobStatus
  ( WorldExportJobStatus
      ( ..,
        WorldExportJobStatus_Canceled,
        WorldExportJobStatus_Canceling,
        WorldExportJobStatus_Completed,
        WorldExportJobStatus_Failed,
        WorldExportJobStatus_Pending,
        WorldExportJobStatus_Running
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorldExportJobStatus = WorldExportJobStatus'
  { fromWorldExportJobStatus ::
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

pattern WorldExportJobStatus_Canceled :: WorldExportJobStatus
pattern WorldExportJobStatus_Canceled = WorldExportJobStatus' "Canceled"

pattern WorldExportJobStatus_Canceling :: WorldExportJobStatus
pattern WorldExportJobStatus_Canceling = WorldExportJobStatus' "Canceling"

pattern WorldExportJobStatus_Completed :: WorldExportJobStatus
pattern WorldExportJobStatus_Completed = WorldExportJobStatus' "Completed"

pattern WorldExportJobStatus_Failed :: WorldExportJobStatus
pattern WorldExportJobStatus_Failed = WorldExportJobStatus' "Failed"

pattern WorldExportJobStatus_Pending :: WorldExportJobStatus
pattern WorldExportJobStatus_Pending = WorldExportJobStatus' "Pending"

pattern WorldExportJobStatus_Running :: WorldExportJobStatus
pattern WorldExportJobStatus_Running = WorldExportJobStatus' "Running"

{-# COMPLETE
  WorldExportJobStatus_Canceled,
  WorldExportJobStatus_Canceling,
  WorldExportJobStatus_Completed,
  WorldExportJobStatus_Failed,
  WorldExportJobStatus_Pending,
  WorldExportJobStatus_Running,
  WorldExportJobStatus'
  #-}
