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
-- Module      : Amazonka.LexV2Models.Types.ExportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ExportStatus
  ( ExportStatus
      ( ..,
        ExportStatus_Completed,
        ExportStatus_Deleting,
        ExportStatus_Failed,
        ExportStatus_InProgress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ExportStatus = ExportStatus'
  { fromExportStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ExportStatus_Completed :: ExportStatus
pattern ExportStatus_Completed = ExportStatus' "Completed"

pattern ExportStatus_Deleting :: ExportStatus
pattern ExportStatus_Deleting = ExportStatus' "Deleting"

pattern ExportStatus_Failed :: ExportStatus
pattern ExportStatus_Failed = ExportStatus' "Failed"

pattern ExportStatus_InProgress :: ExportStatus
pattern ExportStatus_InProgress = ExportStatus' "InProgress"

{-# COMPLETE
  ExportStatus_Completed,
  ExportStatus_Deleting,
  ExportStatus_Failed,
  ExportStatus_InProgress,
  ExportStatus'
  #-}
