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
-- Module      : Amazonka.CloudFormation.Types.StackStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackStatus
  ( StackStatus
      ( ..,
        StackStatus_CREATE_COMPLETE,
        StackStatus_CREATE_FAILED,
        StackStatus_CREATE_IN_PROGRESS,
        StackStatus_DELETE_COMPLETE,
        StackStatus_DELETE_FAILED,
        StackStatus_DELETE_IN_PROGRESS,
        StackStatus_IMPORT_COMPLETE,
        StackStatus_IMPORT_IN_PROGRESS,
        StackStatus_IMPORT_ROLLBACK_COMPLETE,
        StackStatus_IMPORT_ROLLBACK_FAILED,
        StackStatus_IMPORT_ROLLBACK_IN_PROGRESS,
        StackStatus_REVIEW_IN_PROGRESS,
        StackStatus_ROLLBACK_COMPLETE,
        StackStatus_ROLLBACK_FAILED,
        StackStatus_ROLLBACK_IN_PROGRESS,
        StackStatus_UPDATE_COMPLETE,
        StackStatus_UPDATE_COMPLETE_CLEANUP_IN_PROGRESS,
        StackStatus_UPDATE_FAILED,
        StackStatus_UPDATE_IN_PROGRESS,
        StackStatus_UPDATE_ROLLBACK_COMPLETE,
        StackStatus_UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS,
        StackStatus_UPDATE_ROLLBACK_FAILED,
        StackStatus_UPDATE_ROLLBACK_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackStatus = StackStatus'
  { fromStackStatus ::
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

pattern StackStatus_CREATE_COMPLETE :: StackStatus
pattern StackStatus_CREATE_COMPLETE = StackStatus' "CREATE_COMPLETE"

pattern StackStatus_CREATE_FAILED :: StackStatus
pattern StackStatus_CREATE_FAILED = StackStatus' "CREATE_FAILED"

pattern StackStatus_CREATE_IN_PROGRESS :: StackStatus
pattern StackStatus_CREATE_IN_PROGRESS = StackStatus' "CREATE_IN_PROGRESS"

pattern StackStatus_DELETE_COMPLETE :: StackStatus
pattern StackStatus_DELETE_COMPLETE = StackStatus' "DELETE_COMPLETE"

pattern StackStatus_DELETE_FAILED :: StackStatus
pattern StackStatus_DELETE_FAILED = StackStatus' "DELETE_FAILED"

pattern StackStatus_DELETE_IN_PROGRESS :: StackStatus
pattern StackStatus_DELETE_IN_PROGRESS = StackStatus' "DELETE_IN_PROGRESS"

pattern StackStatus_IMPORT_COMPLETE :: StackStatus
pattern StackStatus_IMPORT_COMPLETE = StackStatus' "IMPORT_COMPLETE"

pattern StackStatus_IMPORT_IN_PROGRESS :: StackStatus
pattern StackStatus_IMPORT_IN_PROGRESS = StackStatus' "IMPORT_IN_PROGRESS"

pattern StackStatus_IMPORT_ROLLBACK_COMPLETE :: StackStatus
pattern StackStatus_IMPORT_ROLLBACK_COMPLETE = StackStatus' "IMPORT_ROLLBACK_COMPLETE"

pattern StackStatus_IMPORT_ROLLBACK_FAILED :: StackStatus
pattern StackStatus_IMPORT_ROLLBACK_FAILED = StackStatus' "IMPORT_ROLLBACK_FAILED"

pattern StackStatus_IMPORT_ROLLBACK_IN_PROGRESS :: StackStatus
pattern StackStatus_IMPORT_ROLLBACK_IN_PROGRESS = StackStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern StackStatus_REVIEW_IN_PROGRESS :: StackStatus
pattern StackStatus_REVIEW_IN_PROGRESS = StackStatus' "REVIEW_IN_PROGRESS"

pattern StackStatus_ROLLBACK_COMPLETE :: StackStatus
pattern StackStatus_ROLLBACK_COMPLETE = StackStatus' "ROLLBACK_COMPLETE"

pattern StackStatus_ROLLBACK_FAILED :: StackStatus
pattern StackStatus_ROLLBACK_FAILED = StackStatus' "ROLLBACK_FAILED"

pattern StackStatus_ROLLBACK_IN_PROGRESS :: StackStatus
pattern StackStatus_ROLLBACK_IN_PROGRESS = StackStatus' "ROLLBACK_IN_PROGRESS"

pattern StackStatus_UPDATE_COMPLETE :: StackStatus
pattern StackStatus_UPDATE_COMPLETE = StackStatus' "UPDATE_COMPLETE"

pattern StackStatus_UPDATE_COMPLETE_CLEANUP_IN_PROGRESS :: StackStatus
pattern StackStatus_UPDATE_COMPLETE_CLEANUP_IN_PROGRESS = StackStatus' "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"

pattern StackStatus_UPDATE_FAILED :: StackStatus
pattern StackStatus_UPDATE_FAILED = StackStatus' "UPDATE_FAILED"

pattern StackStatus_UPDATE_IN_PROGRESS :: StackStatus
pattern StackStatus_UPDATE_IN_PROGRESS = StackStatus' "UPDATE_IN_PROGRESS"

pattern StackStatus_UPDATE_ROLLBACK_COMPLETE :: StackStatus
pattern StackStatus_UPDATE_ROLLBACK_COMPLETE = StackStatus' "UPDATE_ROLLBACK_COMPLETE"

pattern StackStatus_UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS :: StackStatus
pattern StackStatus_UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS = StackStatus' "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"

pattern StackStatus_UPDATE_ROLLBACK_FAILED :: StackStatus
pattern StackStatus_UPDATE_ROLLBACK_FAILED = StackStatus' "UPDATE_ROLLBACK_FAILED"

pattern StackStatus_UPDATE_ROLLBACK_IN_PROGRESS :: StackStatus
pattern StackStatus_UPDATE_ROLLBACK_IN_PROGRESS = StackStatus' "UPDATE_ROLLBACK_IN_PROGRESS"

{-# COMPLETE
  StackStatus_CREATE_COMPLETE,
  StackStatus_CREATE_FAILED,
  StackStatus_CREATE_IN_PROGRESS,
  StackStatus_DELETE_COMPLETE,
  StackStatus_DELETE_FAILED,
  StackStatus_DELETE_IN_PROGRESS,
  StackStatus_IMPORT_COMPLETE,
  StackStatus_IMPORT_IN_PROGRESS,
  StackStatus_IMPORT_ROLLBACK_COMPLETE,
  StackStatus_IMPORT_ROLLBACK_FAILED,
  StackStatus_IMPORT_ROLLBACK_IN_PROGRESS,
  StackStatus_REVIEW_IN_PROGRESS,
  StackStatus_ROLLBACK_COMPLETE,
  StackStatus_ROLLBACK_FAILED,
  StackStatus_ROLLBACK_IN_PROGRESS,
  StackStatus_UPDATE_COMPLETE,
  StackStatus_UPDATE_COMPLETE_CLEANUP_IN_PROGRESS,
  StackStatus_UPDATE_FAILED,
  StackStatus_UPDATE_IN_PROGRESS,
  StackStatus_UPDATE_ROLLBACK_COMPLETE,
  StackStatus_UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS,
  StackStatus_UPDATE_ROLLBACK_FAILED,
  StackStatus_UPDATE_ROLLBACK_IN_PROGRESS,
  StackStatus'
  #-}
