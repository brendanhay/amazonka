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
-- Module      : Amazonka.IoTWireless.Types.ImportTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ImportTaskStatus
  ( ImportTaskStatus
      ( ..,
        ImportTaskStatus_COMPLETE,
        ImportTaskStatus_DELETING,
        ImportTaskStatus_FAILED,
        ImportTaskStatus_INITIALIZED,
        ImportTaskStatus_INITIALIZING,
        ImportTaskStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImportTaskStatus = ImportTaskStatus'
  { fromImportTaskStatus ::
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

pattern ImportTaskStatus_COMPLETE :: ImportTaskStatus
pattern ImportTaskStatus_COMPLETE = ImportTaskStatus' "COMPLETE"

pattern ImportTaskStatus_DELETING :: ImportTaskStatus
pattern ImportTaskStatus_DELETING = ImportTaskStatus' "DELETING"

pattern ImportTaskStatus_FAILED :: ImportTaskStatus
pattern ImportTaskStatus_FAILED = ImportTaskStatus' "FAILED"

pattern ImportTaskStatus_INITIALIZED :: ImportTaskStatus
pattern ImportTaskStatus_INITIALIZED = ImportTaskStatus' "INITIALIZED"

pattern ImportTaskStatus_INITIALIZING :: ImportTaskStatus
pattern ImportTaskStatus_INITIALIZING = ImportTaskStatus' "INITIALIZING"

pattern ImportTaskStatus_PENDING :: ImportTaskStatus
pattern ImportTaskStatus_PENDING = ImportTaskStatus' "PENDING"

{-# COMPLETE
  ImportTaskStatus_COMPLETE,
  ImportTaskStatus_DELETING,
  ImportTaskStatus_FAILED,
  ImportTaskStatus_INITIALIZED,
  ImportTaskStatus_INITIALIZING,
  ImportTaskStatus_PENDING,
  ImportTaskStatus'
  #-}
