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
-- Module      : Amazonka.IoTDeviceAdvisor.Types.Status
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types.Status
  ( Status
      ( ..,
        Status_CANCELED,
        Status_ERROR,
        Status_FAIL,
        Status_PASS,
        Status_PASS_WITH_WARNINGS,
        Status_PENDING,
        Status_RUNNING,
        Status_STOPPED,
        Status_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Status = Status' {fromStatus :: Data.Text}
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

pattern Status_CANCELED :: Status
pattern Status_CANCELED = Status' "CANCELED"

pattern Status_ERROR :: Status
pattern Status_ERROR = Status' "ERROR"

pattern Status_FAIL :: Status
pattern Status_FAIL = Status' "FAIL"

pattern Status_PASS :: Status
pattern Status_PASS = Status' "PASS"

pattern Status_PASS_WITH_WARNINGS :: Status
pattern Status_PASS_WITH_WARNINGS = Status' "PASS_WITH_WARNINGS"

pattern Status_PENDING :: Status
pattern Status_PENDING = Status' "PENDING"

pattern Status_RUNNING :: Status
pattern Status_RUNNING = Status' "RUNNING"

pattern Status_STOPPED :: Status
pattern Status_STOPPED = Status' "STOPPED"

pattern Status_STOPPING :: Status
pattern Status_STOPPING = Status' "STOPPING"

{-# COMPLETE
  Status_CANCELED,
  Status_ERROR,
  Status_FAIL,
  Status_PASS,
  Status_PASS_WITH_WARNINGS,
  Status_PENDING,
  Status_RUNNING,
  Status_STOPPED,
  Status_STOPPING,
  Status'
  #-}
