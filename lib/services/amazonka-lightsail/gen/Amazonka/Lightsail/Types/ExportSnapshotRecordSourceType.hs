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
-- Module      : Amazonka.Lightsail.Types.ExportSnapshotRecordSourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ExportSnapshotRecordSourceType
  ( ExportSnapshotRecordSourceType
      ( ..,
        ExportSnapshotRecordSourceType_DiskSnapshot,
        ExportSnapshotRecordSourceType_InstanceSnapshot
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExportSnapshotRecordSourceType = ExportSnapshotRecordSourceType'
  { fromExportSnapshotRecordSourceType ::
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

pattern ExportSnapshotRecordSourceType_DiskSnapshot :: ExportSnapshotRecordSourceType
pattern ExportSnapshotRecordSourceType_DiskSnapshot = ExportSnapshotRecordSourceType' "DiskSnapshot"

pattern ExportSnapshotRecordSourceType_InstanceSnapshot :: ExportSnapshotRecordSourceType
pattern ExportSnapshotRecordSourceType_InstanceSnapshot = ExportSnapshotRecordSourceType' "InstanceSnapshot"

{-# COMPLETE
  ExportSnapshotRecordSourceType_DiskSnapshot,
  ExportSnapshotRecordSourceType_InstanceSnapshot,
  ExportSnapshotRecordSourceType'
  #-}
