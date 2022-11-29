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
-- Module      : Amazonka.SSM.Types.OpsItemFilterKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemFilterKey
  ( OpsItemFilterKey
      ( ..,
        OpsItemFilterKey_AccountId,
        OpsItemFilterKey_ActualEndTime,
        OpsItemFilterKey_ActualStartTime,
        OpsItemFilterKey_AutomationId,
        OpsItemFilterKey_Category,
        OpsItemFilterKey_ChangeRequestByApproverArn,
        OpsItemFilterKey_ChangeRequestByApproverName,
        OpsItemFilterKey_ChangeRequestByRequesterArn,
        OpsItemFilterKey_ChangeRequestByRequesterName,
        OpsItemFilterKey_ChangeRequestByTargetsResourceGroup,
        OpsItemFilterKey_ChangeRequestByTemplate,
        OpsItemFilterKey_CreatedBy,
        OpsItemFilterKey_CreatedTime,
        OpsItemFilterKey_InsightByType,
        OpsItemFilterKey_LastModifiedTime,
        OpsItemFilterKey_OperationalData,
        OpsItemFilterKey_OperationalDataKey,
        OpsItemFilterKey_OperationalDataValue,
        OpsItemFilterKey_OpsItemId,
        OpsItemFilterKey_OpsItemType,
        OpsItemFilterKey_PlannedEndTime,
        OpsItemFilterKey_PlannedStartTime,
        OpsItemFilterKey_Priority,
        OpsItemFilterKey_ResourceId,
        OpsItemFilterKey_Severity,
        OpsItemFilterKey_Source,
        OpsItemFilterKey_Status,
        OpsItemFilterKey_Title
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OpsItemFilterKey = OpsItemFilterKey'
  { fromOpsItemFilterKey ::
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

pattern OpsItemFilterKey_AccountId :: OpsItemFilterKey
pattern OpsItemFilterKey_AccountId = OpsItemFilterKey' "AccountId"

pattern OpsItemFilterKey_ActualEndTime :: OpsItemFilterKey
pattern OpsItemFilterKey_ActualEndTime = OpsItemFilterKey' "ActualEndTime"

pattern OpsItemFilterKey_ActualStartTime :: OpsItemFilterKey
pattern OpsItemFilterKey_ActualStartTime = OpsItemFilterKey' "ActualStartTime"

pattern OpsItemFilterKey_AutomationId :: OpsItemFilterKey
pattern OpsItemFilterKey_AutomationId = OpsItemFilterKey' "AutomationId"

pattern OpsItemFilterKey_Category :: OpsItemFilterKey
pattern OpsItemFilterKey_Category = OpsItemFilterKey' "Category"

pattern OpsItemFilterKey_ChangeRequestByApproverArn :: OpsItemFilterKey
pattern OpsItemFilterKey_ChangeRequestByApproverArn = OpsItemFilterKey' "ChangeRequestByApproverArn"

pattern OpsItemFilterKey_ChangeRequestByApproverName :: OpsItemFilterKey
pattern OpsItemFilterKey_ChangeRequestByApproverName = OpsItemFilterKey' "ChangeRequestByApproverName"

pattern OpsItemFilterKey_ChangeRequestByRequesterArn :: OpsItemFilterKey
pattern OpsItemFilterKey_ChangeRequestByRequesterArn = OpsItemFilterKey' "ChangeRequestByRequesterArn"

pattern OpsItemFilterKey_ChangeRequestByRequesterName :: OpsItemFilterKey
pattern OpsItemFilterKey_ChangeRequestByRequesterName = OpsItemFilterKey' "ChangeRequestByRequesterName"

pattern OpsItemFilterKey_ChangeRequestByTargetsResourceGroup :: OpsItemFilterKey
pattern OpsItemFilterKey_ChangeRequestByTargetsResourceGroup = OpsItemFilterKey' "ChangeRequestByTargetsResourceGroup"

pattern OpsItemFilterKey_ChangeRequestByTemplate :: OpsItemFilterKey
pattern OpsItemFilterKey_ChangeRequestByTemplate = OpsItemFilterKey' "ChangeRequestByTemplate"

pattern OpsItemFilterKey_CreatedBy :: OpsItemFilterKey
pattern OpsItemFilterKey_CreatedBy = OpsItemFilterKey' "CreatedBy"

pattern OpsItemFilterKey_CreatedTime :: OpsItemFilterKey
pattern OpsItemFilterKey_CreatedTime = OpsItemFilterKey' "CreatedTime"

pattern OpsItemFilterKey_InsightByType :: OpsItemFilterKey
pattern OpsItemFilterKey_InsightByType = OpsItemFilterKey' "InsightByType"

pattern OpsItemFilterKey_LastModifiedTime :: OpsItemFilterKey
pattern OpsItemFilterKey_LastModifiedTime = OpsItemFilterKey' "LastModifiedTime"

pattern OpsItemFilterKey_OperationalData :: OpsItemFilterKey
pattern OpsItemFilterKey_OperationalData = OpsItemFilterKey' "OperationalData"

pattern OpsItemFilterKey_OperationalDataKey :: OpsItemFilterKey
pattern OpsItemFilterKey_OperationalDataKey = OpsItemFilterKey' "OperationalDataKey"

pattern OpsItemFilterKey_OperationalDataValue :: OpsItemFilterKey
pattern OpsItemFilterKey_OperationalDataValue = OpsItemFilterKey' "OperationalDataValue"

pattern OpsItemFilterKey_OpsItemId :: OpsItemFilterKey
pattern OpsItemFilterKey_OpsItemId = OpsItemFilterKey' "OpsItemId"

pattern OpsItemFilterKey_OpsItemType :: OpsItemFilterKey
pattern OpsItemFilterKey_OpsItemType = OpsItemFilterKey' "OpsItemType"

pattern OpsItemFilterKey_PlannedEndTime :: OpsItemFilterKey
pattern OpsItemFilterKey_PlannedEndTime = OpsItemFilterKey' "PlannedEndTime"

pattern OpsItemFilterKey_PlannedStartTime :: OpsItemFilterKey
pattern OpsItemFilterKey_PlannedStartTime = OpsItemFilterKey' "PlannedStartTime"

pattern OpsItemFilterKey_Priority :: OpsItemFilterKey
pattern OpsItemFilterKey_Priority = OpsItemFilterKey' "Priority"

pattern OpsItemFilterKey_ResourceId :: OpsItemFilterKey
pattern OpsItemFilterKey_ResourceId = OpsItemFilterKey' "ResourceId"

pattern OpsItemFilterKey_Severity :: OpsItemFilterKey
pattern OpsItemFilterKey_Severity = OpsItemFilterKey' "Severity"

pattern OpsItemFilterKey_Source :: OpsItemFilterKey
pattern OpsItemFilterKey_Source = OpsItemFilterKey' "Source"

pattern OpsItemFilterKey_Status :: OpsItemFilterKey
pattern OpsItemFilterKey_Status = OpsItemFilterKey' "Status"

pattern OpsItemFilterKey_Title :: OpsItemFilterKey
pattern OpsItemFilterKey_Title = OpsItemFilterKey' "Title"

{-# COMPLETE
  OpsItemFilterKey_AccountId,
  OpsItemFilterKey_ActualEndTime,
  OpsItemFilterKey_ActualStartTime,
  OpsItemFilterKey_AutomationId,
  OpsItemFilterKey_Category,
  OpsItemFilterKey_ChangeRequestByApproverArn,
  OpsItemFilterKey_ChangeRequestByApproverName,
  OpsItemFilterKey_ChangeRequestByRequesterArn,
  OpsItemFilterKey_ChangeRequestByRequesterName,
  OpsItemFilterKey_ChangeRequestByTargetsResourceGroup,
  OpsItemFilterKey_ChangeRequestByTemplate,
  OpsItemFilterKey_CreatedBy,
  OpsItemFilterKey_CreatedTime,
  OpsItemFilterKey_InsightByType,
  OpsItemFilterKey_LastModifiedTime,
  OpsItemFilterKey_OperationalData,
  OpsItemFilterKey_OperationalDataKey,
  OpsItemFilterKey_OperationalDataValue,
  OpsItemFilterKey_OpsItemId,
  OpsItemFilterKey_OpsItemType,
  OpsItemFilterKey_PlannedEndTime,
  OpsItemFilterKey_PlannedStartTime,
  OpsItemFilterKey_Priority,
  OpsItemFilterKey_ResourceId,
  OpsItemFilterKey_Severity,
  OpsItemFilterKey_Source,
  OpsItemFilterKey_Status,
  OpsItemFilterKey_Title,
  OpsItemFilterKey'
  #-}
