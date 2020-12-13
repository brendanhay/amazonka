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
        OIFKStatus,
        OIFKCreatedBy,
        OIFKSource,
        OIFKPriority,
        OIFKTitle,
        OIFKOpsItemId,
        OIFKCreatedTime,
        OIFKLastModifiedTime,
        OIFKOperationalData,
        OIFKOperationalDataKey,
        OIFKOperationalDataValue,
        OIFKResourceId,
        OIFKAutomationId,
        OIFKCategory,
        OIFKSeverity
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OpsItemFilterKey = OpsItemFilterKey' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OIFKStatus :: OpsItemFilterKey
pattern OIFKStatus = OpsItemFilterKey' "Status"

pattern OIFKCreatedBy :: OpsItemFilterKey
pattern OIFKCreatedBy = OpsItemFilterKey' "CreatedBy"

pattern OIFKSource :: OpsItemFilterKey
pattern OIFKSource = OpsItemFilterKey' "Source"

pattern OIFKPriority :: OpsItemFilterKey
pattern OIFKPriority = OpsItemFilterKey' "Priority"

pattern OIFKTitle :: OpsItemFilterKey
pattern OIFKTitle = OpsItemFilterKey' "Title"

pattern OIFKOpsItemId :: OpsItemFilterKey
pattern OIFKOpsItemId = OpsItemFilterKey' "OpsItemId"

pattern OIFKCreatedTime :: OpsItemFilterKey
pattern OIFKCreatedTime = OpsItemFilterKey' "CreatedTime"

pattern OIFKLastModifiedTime :: OpsItemFilterKey
pattern OIFKLastModifiedTime = OpsItemFilterKey' "LastModifiedTime"

pattern OIFKOperationalData :: OpsItemFilterKey
pattern OIFKOperationalData = OpsItemFilterKey' "OperationalData"

pattern OIFKOperationalDataKey :: OpsItemFilterKey
pattern OIFKOperationalDataKey = OpsItemFilterKey' "OperationalDataKey"

pattern OIFKOperationalDataValue :: OpsItemFilterKey
pattern OIFKOperationalDataValue = OpsItemFilterKey' "OperationalDataValue"

pattern OIFKResourceId :: OpsItemFilterKey
pattern OIFKResourceId = OpsItemFilterKey' "ResourceId"

pattern OIFKAutomationId :: OpsItemFilterKey
pattern OIFKAutomationId = OpsItemFilterKey' "AutomationId"

pattern OIFKCategory :: OpsItemFilterKey
pattern OIFKCategory = OpsItemFilterKey' "Category"

pattern OIFKSeverity :: OpsItemFilterKey
pattern OIFKSeverity = OpsItemFilterKey' "Severity"

{-# COMPLETE
  OIFKStatus,
  OIFKCreatedBy,
  OIFKSource,
  OIFKPriority,
  OIFKTitle,
  OIFKOpsItemId,
  OIFKCreatedTime,
  OIFKLastModifiedTime,
  OIFKOperationalData,
  OIFKOperationalDataKey,
  OIFKOperationalDataValue,
  OIFKResourceId,
  OIFKAutomationId,
  OIFKCategory,
  OIFKSeverity,
  OpsItemFilterKey'
  #-}
