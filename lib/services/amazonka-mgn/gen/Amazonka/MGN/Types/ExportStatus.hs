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
-- Module      : Amazonka.MGN.Types.ExportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ExportStatus
  ( ExportStatus
      ( ..,
        ExportStatus_FAILED,
        ExportStatus_PENDING,
        ExportStatus_STARTED,
        ExportStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExportStatus = ExportStatus'
  { fromExportStatus ::
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

pattern ExportStatus_FAILED :: ExportStatus
pattern ExportStatus_FAILED = ExportStatus' "FAILED"

pattern ExportStatus_PENDING :: ExportStatus
pattern ExportStatus_PENDING = ExportStatus' "PENDING"

pattern ExportStatus_STARTED :: ExportStatus
pattern ExportStatus_STARTED = ExportStatus' "STARTED"

pattern ExportStatus_SUCCEEDED :: ExportStatus
pattern ExportStatus_SUCCEEDED = ExportStatus' "SUCCEEDED"

{-# COMPLETE
  ExportStatus_FAILED,
  ExportStatus_PENDING,
  ExportStatus_STARTED,
  ExportStatus_SUCCEEDED,
  ExportStatus'
  #-}
