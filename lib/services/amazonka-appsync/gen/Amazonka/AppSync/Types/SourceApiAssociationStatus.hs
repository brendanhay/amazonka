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
-- Module      : Amazonka.AppSync.Types.SourceApiAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.SourceApiAssociationStatus
  ( SourceApiAssociationStatus
      ( ..,
        SourceApiAssociationStatus_AUTO_MERGE_SCHEDULE_FAILED,
        SourceApiAssociationStatus_DELETION_FAILED,
        SourceApiAssociationStatus_DELETION_IN_PROGRESS,
        SourceApiAssociationStatus_DELETION_SCHEDULED,
        SourceApiAssociationStatus_MERGE_FAILED,
        SourceApiAssociationStatus_MERGE_IN_PROGRESS,
        SourceApiAssociationStatus_MERGE_SCHEDULED,
        SourceApiAssociationStatus_MERGE_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceApiAssociationStatus = SourceApiAssociationStatus'
  { fromSourceApiAssociationStatus ::
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

pattern SourceApiAssociationStatus_AUTO_MERGE_SCHEDULE_FAILED :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_AUTO_MERGE_SCHEDULE_FAILED = SourceApiAssociationStatus' "AUTO_MERGE_SCHEDULE_FAILED"

pattern SourceApiAssociationStatus_DELETION_FAILED :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_DELETION_FAILED = SourceApiAssociationStatus' "DELETION_FAILED"

pattern SourceApiAssociationStatus_DELETION_IN_PROGRESS :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_DELETION_IN_PROGRESS = SourceApiAssociationStatus' "DELETION_IN_PROGRESS"

pattern SourceApiAssociationStatus_DELETION_SCHEDULED :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_DELETION_SCHEDULED = SourceApiAssociationStatus' "DELETION_SCHEDULED"

pattern SourceApiAssociationStatus_MERGE_FAILED :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_MERGE_FAILED = SourceApiAssociationStatus' "MERGE_FAILED"

pattern SourceApiAssociationStatus_MERGE_IN_PROGRESS :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_MERGE_IN_PROGRESS = SourceApiAssociationStatus' "MERGE_IN_PROGRESS"

pattern SourceApiAssociationStatus_MERGE_SCHEDULED :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_MERGE_SCHEDULED = SourceApiAssociationStatus' "MERGE_SCHEDULED"

pattern SourceApiAssociationStatus_MERGE_SUCCESS :: SourceApiAssociationStatus
pattern SourceApiAssociationStatus_MERGE_SUCCESS = SourceApiAssociationStatus' "MERGE_SUCCESS"

{-# COMPLETE
  SourceApiAssociationStatus_AUTO_MERGE_SCHEDULE_FAILED,
  SourceApiAssociationStatus_DELETION_FAILED,
  SourceApiAssociationStatus_DELETION_IN_PROGRESS,
  SourceApiAssociationStatus_DELETION_SCHEDULED,
  SourceApiAssociationStatus_MERGE_FAILED,
  SourceApiAssociationStatus_MERGE_IN_PROGRESS,
  SourceApiAssociationStatus_MERGE_SCHEDULED,
  SourceApiAssociationStatus_MERGE_SUCCESS,
  SourceApiAssociationStatus'
  #-}
