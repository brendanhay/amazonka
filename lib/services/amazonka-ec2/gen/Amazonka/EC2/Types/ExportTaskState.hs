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
-- Module      : Amazonka.EC2.Types.ExportTaskState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ExportTaskState
  ( ExportTaskState
      ( ..,
        ExportTaskState_Active,
        ExportTaskState_Cancelled,
        ExportTaskState_Cancelling,
        ExportTaskState_Completed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ExportTaskState = ExportTaskState'
  { fromExportTaskState ::
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

pattern ExportTaskState_Active :: ExportTaskState
pattern ExportTaskState_Active = ExportTaskState' "active"

pattern ExportTaskState_Cancelled :: ExportTaskState
pattern ExportTaskState_Cancelled = ExportTaskState' "cancelled"

pattern ExportTaskState_Cancelling :: ExportTaskState
pattern ExportTaskState_Cancelling = ExportTaskState' "cancelling"

pattern ExportTaskState_Completed :: ExportTaskState
pattern ExportTaskState_Completed = ExportTaskState' "completed"

{-# COMPLETE
  ExportTaskState_Active,
  ExportTaskState_Cancelled,
  ExportTaskState_Cancelling,
  ExportTaskState_Completed,
  ExportTaskState'
  #-}
