{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilterKey
  ( OpsItemFilterKey
      ( OpsItemFilterKey',
        OpsItemFilterKeyStatus,
        OpsItemFilterKeyCreatedBy,
        OpsItemFilterKeySource,
        OpsItemFilterKeyPriority,
        OpsItemFilterKeyTitle,
        OpsItemFilterKeyOpsItemId,
        OpsItemFilterKeyCreatedTime,
        OpsItemFilterKeyLastModifiedTime,
        OpsItemFilterKeyOperationalData,
        OpsItemFilterKeyOperationalDataKey,
        OpsItemFilterKeyOperationalDataValue,
        OpsItemFilterKeyResourceId,
        OpsItemFilterKeyAutomationId,
        OpsItemFilterKeyCategory,
        OpsItemFilterKeySeverity,
        fromOpsItemFilterKey
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OpsItemFilterKey = OpsItemFilterKey'
  { fromOpsItemFilterKey ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern OpsItemFilterKeyStatus :: OpsItemFilterKey
pattern OpsItemFilterKeyStatus = OpsItemFilterKey' "Status"

pattern OpsItemFilterKeyCreatedBy :: OpsItemFilterKey
pattern OpsItemFilterKeyCreatedBy = OpsItemFilterKey' "CreatedBy"

pattern OpsItemFilterKeySource :: OpsItemFilterKey
pattern OpsItemFilterKeySource = OpsItemFilterKey' "Source"

pattern OpsItemFilterKeyPriority :: OpsItemFilterKey
pattern OpsItemFilterKeyPriority = OpsItemFilterKey' "Priority"

pattern OpsItemFilterKeyTitle :: OpsItemFilterKey
pattern OpsItemFilterKeyTitle = OpsItemFilterKey' "Title"

pattern OpsItemFilterKeyOpsItemId :: OpsItemFilterKey
pattern OpsItemFilterKeyOpsItemId = OpsItemFilterKey' "OpsItemId"

pattern OpsItemFilterKeyCreatedTime :: OpsItemFilterKey
pattern OpsItemFilterKeyCreatedTime = OpsItemFilterKey' "CreatedTime"

pattern OpsItemFilterKeyLastModifiedTime :: OpsItemFilterKey
pattern OpsItemFilterKeyLastModifiedTime = OpsItemFilterKey' "LastModifiedTime"

pattern OpsItemFilterKeyOperationalData :: OpsItemFilterKey
pattern OpsItemFilterKeyOperationalData = OpsItemFilterKey' "OperationalData"

pattern OpsItemFilterKeyOperationalDataKey :: OpsItemFilterKey
pattern OpsItemFilterKeyOperationalDataKey = OpsItemFilterKey' "OperationalDataKey"

pattern OpsItemFilterKeyOperationalDataValue :: OpsItemFilterKey
pattern OpsItemFilterKeyOperationalDataValue = OpsItemFilterKey' "OperationalDataValue"

pattern OpsItemFilterKeyResourceId :: OpsItemFilterKey
pattern OpsItemFilterKeyResourceId = OpsItemFilterKey' "ResourceId"

pattern OpsItemFilterKeyAutomationId :: OpsItemFilterKey
pattern OpsItemFilterKeyAutomationId = OpsItemFilterKey' "AutomationId"

pattern OpsItemFilterKeyCategory :: OpsItemFilterKey
pattern OpsItemFilterKeyCategory = OpsItemFilterKey' "Category"

pattern OpsItemFilterKeySeverity :: OpsItemFilterKey
pattern OpsItemFilterKeySeverity = OpsItemFilterKey' "Severity"

{-# COMPLETE
  OpsItemFilterKeyStatus,
  OpsItemFilterKeyCreatedBy,
  OpsItemFilterKeySource,
  OpsItemFilterKeyPriority,
  OpsItemFilterKeyTitle,
  OpsItemFilterKeyOpsItemId,
  OpsItemFilterKeyCreatedTime,
  OpsItemFilterKeyLastModifiedTime,
  OpsItemFilterKeyOperationalData,
  OpsItemFilterKeyOperationalDataKey,
  OpsItemFilterKeyOperationalDataValue,
  OpsItemFilterKeyResourceId,
  OpsItemFilterKeyAutomationId,
  OpsItemFilterKeyCategory,
  OpsItemFilterKeySeverity,
  OpsItemFilterKey'
  #-}
