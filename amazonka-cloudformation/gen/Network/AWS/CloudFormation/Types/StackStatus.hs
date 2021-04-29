{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackStatus
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
        StackStatus_UPDATE_IN_PROGRESS,
        StackStatus_UPDATE_ROLLBACK_COMPLETE,
        StackStatus_UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS,
        StackStatus_UPDATE_ROLLBACK_FAILED,
        StackStatus_UPDATE_ROLLBACK_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StackStatus = StackStatus'
  { fromStackStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
  StackStatus_UPDATE_IN_PROGRESS,
  StackStatus_UPDATE_ROLLBACK_COMPLETE,
  StackStatus_UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS,
  StackStatus_UPDATE_ROLLBACK_FAILED,
  StackStatus_UPDATE_ROLLBACK_IN_PROGRESS,
  StackStatus'
  #-}
