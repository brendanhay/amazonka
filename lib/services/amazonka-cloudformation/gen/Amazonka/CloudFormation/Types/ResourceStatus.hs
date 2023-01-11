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
-- Module      : Amazonka.CloudFormation.Types.ResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ResourceStatus
  ( ResourceStatus
      ( ..,
        ResourceStatus_CREATE_COMPLETE,
        ResourceStatus_CREATE_FAILED,
        ResourceStatus_CREATE_IN_PROGRESS,
        ResourceStatus_DELETE_COMPLETE,
        ResourceStatus_DELETE_FAILED,
        ResourceStatus_DELETE_IN_PROGRESS,
        ResourceStatus_DELETE_SKIPPED,
        ResourceStatus_IMPORT_COMPLETE,
        ResourceStatus_IMPORT_FAILED,
        ResourceStatus_IMPORT_IN_PROGRESS,
        ResourceStatus_IMPORT_ROLLBACK_COMPLETE,
        ResourceStatus_IMPORT_ROLLBACK_FAILED,
        ResourceStatus_IMPORT_ROLLBACK_IN_PROGRESS,
        ResourceStatus_ROLLBACK_COMPLETE,
        ResourceStatus_ROLLBACK_FAILED,
        ResourceStatus_ROLLBACK_IN_PROGRESS,
        ResourceStatus_UPDATE_COMPLETE,
        ResourceStatus_UPDATE_FAILED,
        ResourceStatus_UPDATE_IN_PROGRESS,
        ResourceStatus_UPDATE_ROLLBACK_COMPLETE,
        ResourceStatus_UPDATE_ROLLBACK_FAILED,
        ResourceStatus_UPDATE_ROLLBACK_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceStatus = ResourceStatus'
  { fromResourceStatus ::
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

pattern ResourceStatus_CREATE_COMPLETE :: ResourceStatus
pattern ResourceStatus_CREATE_COMPLETE = ResourceStatus' "CREATE_COMPLETE"

pattern ResourceStatus_CREATE_FAILED :: ResourceStatus
pattern ResourceStatus_CREATE_FAILED = ResourceStatus' "CREATE_FAILED"

pattern ResourceStatus_CREATE_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_CREATE_IN_PROGRESS = ResourceStatus' "CREATE_IN_PROGRESS"

pattern ResourceStatus_DELETE_COMPLETE :: ResourceStatus
pattern ResourceStatus_DELETE_COMPLETE = ResourceStatus' "DELETE_COMPLETE"

pattern ResourceStatus_DELETE_FAILED :: ResourceStatus
pattern ResourceStatus_DELETE_FAILED = ResourceStatus' "DELETE_FAILED"

pattern ResourceStatus_DELETE_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_DELETE_IN_PROGRESS = ResourceStatus' "DELETE_IN_PROGRESS"

pattern ResourceStatus_DELETE_SKIPPED :: ResourceStatus
pattern ResourceStatus_DELETE_SKIPPED = ResourceStatus' "DELETE_SKIPPED"

pattern ResourceStatus_IMPORT_COMPLETE :: ResourceStatus
pattern ResourceStatus_IMPORT_COMPLETE = ResourceStatus' "IMPORT_COMPLETE"

pattern ResourceStatus_IMPORT_FAILED :: ResourceStatus
pattern ResourceStatus_IMPORT_FAILED = ResourceStatus' "IMPORT_FAILED"

pattern ResourceStatus_IMPORT_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_IMPORT_IN_PROGRESS = ResourceStatus' "IMPORT_IN_PROGRESS"

pattern ResourceStatus_IMPORT_ROLLBACK_COMPLETE :: ResourceStatus
pattern ResourceStatus_IMPORT_ROLLBACK_COMPLETE = ResourceStatus' "IMPORT_ROLLBACK_COMPLETE"

pattern ResourceStatus_IMPORT_ROLLBACK_FAILED :: ResourceStatus
pattern ResourceStatus_IMPORT_ROLLBACK_FAILED = ResourceStatus' "IMPORT_ROLLBACK_FAILED"

pattern ResourceStatus_IMPORT_ROLLBACK_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_IMPORT_ROLLBACK_IN_PROGRESS = ResourceStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern ResourceStatus_ROLLBACK_COMPLETE :: ResourceStatus
pattern ResourceStatus_ROLLBACK_COMPLETE = ResourceStatus' "ROLLBACK_COMPLETE"

pattern ResourceStatus_ROLLBACK_FAILED :: ResourceStatus
pattern ResourceStatus_ROLLBACK_FAILED = ResourceStatus' "ROLLBACK_FAILED"

pattern ResourceStatus_ROLLBACK_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_ROLLBACK_IN_PROGRESS = ResourceStatus' "ROLLBACK_IN_PROGRESS"

pattern ResourceStatus_UPDATE_COMPLETE :: ResourceStatus
pattern ResourceStatus_UPDATE_COMPLETE = ResourceStatus' "UPDATE_COMPLETE"

pattern ResourceStatus_UPDATE_FAILED :: ResourceStatus
pattern ResourceStatus_UPDATE_FAILED = ResourceStatus' "UPDATE_FAILED"

pattern ResourceStatus_UPDATE_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_UPDATE_IN_PROGRESS = ResourceStatus' "UPDATE_IN_PROGRESS"

pattern ResourceStatus_UPDATE_ROLLBACK_COMPLETE :: ResourceStatus
pattern ResourceStatus_UPDATE_ROLLBACK_COMPLETE = ResourceStatus' "UPDATE_ROLLBACK_COMPLETE"

pattern ResourceStatus_UPDATE_ROLLBACK_FAILED :: ResourceStatus
pattern ResourceStatus_UPDATE_ROLLBACK_FAILED = ResourceStatus' "UPDATE_ROLLBACK_FAILED"

pattern ResourceStatus_UPDATE_ROLLBACK_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_UPDATE_ROLLBACK_IN_PROGRESS = ResourceStatus' "UPDATE_ROLLBACK_IN_PROGRESS"

{-# COMPLETE
  ResourceStatus_CREATE_COMPLETE,
  ResourceStatus_CREATE_FAILED,
  ResourceStatus_CREATE_IN_PROGRESS,
  ResourceStatus_DELETE_COMPLETE,
  ResourceStatus_DELETE_FAILED,
  ResourceStatus_DELETE_IN_PROGRESS,
  ResourceStatus_DELETE_SKIPPED,
  ResourceStatus_IMPORT_COMPLETE,
  ResourceStatus_IMPORT_FAILED,
  ResourceStatus_IMPORT_IN_PROGRESS,
  ResourceStatus_IMPORT_ROLLBACK_COMPLETE,
  ResourceStatus_IMPORT_ROLLBACK_FAILED,
  ResourceStatus_IMPORT_ROLLBACK_IN_PROGRESS,
  ResourceStatus_ROLLBACK_COMPLETE,
  ResourceStatus_ROLLBACK_FAILED,
  ResourceStatus_ROLLBACK_IN_PROGRESS,
  ResourceStatus_UPDATE_COMPLETE,
  ResourceStatus_UPDATE_FAILED,
  ResourceStatus_UPDATE_IN_PROGRESS,
  ResourceStatus_UPDATE_ROLLBACK_COMPLETE,
  ResourceStatus_UPDATE_ROLLBACK_FAILED,
  ResourceStatus_UPDATE_ROLLBACK_IN_PROGRESS,
  ResourceStatus'
  #-}
