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
-- Module      : Amazonka.RobOMaker.Types.WorldGenerationJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldGenerationJobStatus
  ( WorldGenerationJobStatus
      ( ..,
        WorldGenerationJobStatus_Canceled,
        WorldGenerationJobStatus_Canceling,
        WorldGenerationJobStatus_Completed,
        WorldGenerationJobStatus_Failed,
        WorldGenerationJobStatus_PartialFailed,
        WorldGenerationJobStatus_Pending,
        WorldGenerationJobStatus_Running
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorldGenerationJobStatus = WorldGenerationJobStatus'
  { fromWorldGenerationJobStatus ::
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

pattern WorldGenerationJobStatus_Canceled :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_Canceled = WorldGenerationJobStatus' "Canceled"

pattern WorldGenerationJobStatus_Canceling :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_Canceling = WorldGenerationJobStatus' "Canceling"

pattern WorldGenerationJobStatus_Completed :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_Completed = WorldGenerationJobStatus' "Completed"

pattern WorldGenerationJobStatus_Failed :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_Failed = WorldGenerationJobStatus' "Failed"

pattern WorldGenerationJobStatus_PartialFailed :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_PartialFailed = WorldGenerationJobStatus' "PartialFailed"

pattern WorldGenerationJobStatus_Pending :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_Pending = WorldGenerationJobStatus' "Pending"

pattern WorldGenerationJobStatus_Running :: WorldGenerationJobStatus
pattern WorldGenerationJobStatus_Running = WorldGenerationJobStatus' "Running"

{-# COMPLETE
  WorldGenerationJobStatus_Canceled,
  WorldGenerationJobStatus_Canceling,
  WorldGenerationJobStatus_Completed,
  WorldGenerationJobStatus_Failed,
  WorldGenerationJobStatus_PartialFailed,
  WorldGenerationJobStatus_Pending,
  WorldGenerationJobStatus_Running,
  WorldGenerationJobStatus'
  #-}
