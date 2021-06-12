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
-- Module      : Network.AWS.LexModels.Types.ExportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ExportStatus
  ( ExportStatus
      ( ..,
        ExportStatus_FAILED,
        ExportStatus_IN_PROGRESS,
        ExportStatus_READY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ExportStatus = ExportStatus'
  { fromExportStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
