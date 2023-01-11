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
-- Module      : Amazonka.StepFunctions.Types.MapRunStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.MapRunStatus
  ( MapRunStatus
      ( ..,
        MapRunStatus_ABORTED,
        MapRunStatus_FAILED,
        MapRunStatus_RUNNING,
        MapRunStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MapRunStatus = MapRunStatus'
  { fromMapRunStatus ::
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

pattern MapRunStatus_ABORTED :: MapRunStatus
pattern MapRunStatus_ABORTED = MapRunStatus' "ABORTED"

pattern MapRunStatus_FAILED :: MapRunStatus
pattern MapRunStatus_FAILED = MapRunStatus' "FAILED"

pattern MapRunStatus_RUNNING :: MapRunStatus
pattern MapRunStatus_RUNNING = MapRunStatus' "RUNNING"

pattern MapRunStatus_SUCCEEDED :: MapRunStatus
pattern MapRunStatus_SUCCEEDED = MapRunStatus' "SUCCEEDED"

{-# COMPLETE
  MapRunStatus_ABORTED,
  MapRunStatus_FAILED,
  MapRunStatus_RUNNING,
  MapRunStatus_SUCCEEDED,
  MapRunStatus'
  #-}
