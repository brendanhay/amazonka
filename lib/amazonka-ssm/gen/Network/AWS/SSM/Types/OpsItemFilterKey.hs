{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilterKey where

import Network.AWS.Prelude

data OpsItemFilterKey
  = OIFKAutomationId
  | OIFKCategory
  | OIFKCreatedBy
  | OIFKCreatedTime
  | OIFKLastModifiedTime
  | OIFKOperationalData
  | OIFKOperationalDataKey
  | OIFKOperationalDataValue
  | OIFKOpsItemId
  | OIFKPriority
  | OIFKResourceId
  | OIFKSeverity
  | OIFKSource
  | OIFKStatus
  | OIFKTitle
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText OpsItemFilterKey where
  parser =
    takeLowerText >>= \case
      "automationid" -> pure OIFKAutomationId
      "category" -> pure OIFKCategory
      "createdby" -> pure OIFKCreatedBy
      "createdtime" -> pure OIFKCreatedTime
      "lastmodifiedtime" -> pure OIFKLastModifiedTime
      "operationaldata" -> pure OIFKOperationalData
      "operationaldatakey" -> pure OIFKOperationalDataKey
      "operationaldatavalue" -> pure OIFKOperationalDataValue
      "opsitemid" -> pure OIFKOpsItemId
      "priority" -> pure OIFKPriority
      "resourceid" -> pure OIFKResourceId
      "severity" -> pure OIFKSeverity
      "source" -> pure OIFKSource
      "status" -> pure OIFKStatus
      "title" -> pure OIFKTitle
      e ->
        fromTextError $
          "Failure parsing OpsItemFilterKey from value: '" <> e
            <> "'. Accepted values: automationid, category, createdby, createdtime, lastmodifiedtime, operationaldata, operationaldatakey, operationaldatavalue, opsitemid, priority, resourceid, severity, source, status, title"

instance ToText OpsItemFilterKey where
  toText = \case
    OIFKAutomationId -> "AutomationId"
    OIFKCategory -> "Category"
    OIFKCreatedBy -> "CreatedBy"
    OIFKCreatedTime -> "CreatedTime"
    OIFKLastModifiedTime -> "LastModifiedTime"
    OIFKOperationalData -> "OperationalData"
    OIFKOperationalDataKey -> "OperationalDataKey"
    OIFKOperationalDataValue -> "OperationalDataValue"
    OIFKOpsItemId -> "OpsItemId"
    OIFKPriority -> "Priority"
    OIFKResourceId -> "ResourceId"
    OIFKSeverity -> "Severity"
    OIFKSource -> "Source"
    OIFKStatus -> "Status"
    OIFKTitle -> "Title"

instance Hashable OpsItemFilterKey

instance NFData OpsItemFilterKey

instance ToByteString OpsItemFilterKey

instance ToQuery OpsItemFilterKey

instance ToHeader OpsItemFilterKey

instance ToJSON OpsItemFilterKey where
  toJSON = toJSONText
