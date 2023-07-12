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
-- Module      : Amazonka.LexModels.Types.ExportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.ExportStatus
  ( ExportStatus
      ( ..,
        ExportStatus_FAILED,
        ExportStatus_IN_PROGRESS,
        ExportStatus_READY
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

pattern ExportStatus_IN_PROGRESS :: ExportStatus
pattern ExportStatus_IN_PROGRESS = ExportStatus' "IN_PROGRESS"

pattern ExportStatus_READY :: ExportStatus
pattern ExportStatus_READY = ExportStatus' "READY"

{-# COMPLETE
  ExportStatus_FAILED,
  ExportStatus_IN_PROGRESS,
  ExportStatus_READY,
  ExportStatus'
  #-}
