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
-- Module      : Network.AWS.Config.Types.OrganizationRuleStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationRuleStatus
  ( OrganizationRuleStatus
      ( ..,
        OrganizationRuleStatus_CREATE_FAILED,
        OrganizationRuleStatus_CREATE_IN_PROGRESS,
        OrganizationRuleStatus_CREATE_SUCCESSFUL,
        OrganizationRuleStatus_DELETE_FAILED,
        OrganizationRuleStatus_DELETE_IN_PROGRESS,
        OrganizationRuleStatus_DELETE_SUCCESSFUL,
        OrganizationRuleStatus_UPDATE_FAILED,
        OrganizationRuleStatus_UPDATE_IN_PROGRESS,
        OrganizationRuleStatus_UPDATE_SUCCESSFUL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OrganizationRuleStatus = OrganizationRuleStatus'
  { fromOrganizationRuleStatus ::
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

pattern OrganizationRuleStatus_CREATE_FAILED :: OrganizationRuleStatus
pattern OrganizationRuleStatus_CREATE_FAILED = OrganizationRuleStatus' "CREATE_FAILED"

pattern OrganizationRuleStatus_CREATE_IN_PROGRESS :: OrganizationRuleStatus
pattern OrganizationRuleStatus_CREATE_IN_PROGRESS = OrganizationRuleStatus' "CREATE_IN_PROGRESS"

pattern OrganizationRuleStatus_CREATE_SUCCESSFUL :: OrganizationRuleStatus
pattern OrganizationRuleStatus_CREATE_SUCCESSFUL = OrganizationRuleStatus' "CREATE_SUCCESSFUL"

pattern OrganizationRuleStatus_DELETE_FAILED :: OrganizationRuleStatus
pattern OrganizationRuleStatus_DELETE_FAILED = OrganizationRuleStatus' "DELETE_FAILED"

pattern OrganizationRuleStatus_DELETE_IN_PROGRESS :: OrganizationRuleStatus
pattern OrganizationRuleStatus_DELETE_IN_PROGRESS = OrganizationRuleStatus' "DELETE_IN_PROGRESS"

pattern OrganizationRuleStatus_DELETE_SUCCESSFUL :: OrganizationRuleStatus
pattern OrganizationRuleStatus_DELETE_SUCCESSFUL = OrganizationRuleStatus' "DELETE_SUCCESSFUL"

pattern OrganizationRuleStatus_UPDATE_FAILED :: OrganizationRuleStatus
pattern OrganizationRuleStatus_UPDATE_FAILED = OrganizationRuleStatus' "UPDATE_FAILED"

pattern OrganizationRuleStatus_UPDATE_IN_PROGRESS :: OrganizationRuleStatus
pattern OrganizationRuleStatus_UPDATE_IN_PROGRESS = OrganizationRuleStatus' "UPDATE_IN_PROGRESS"

pattern OrganizationRuleStatus_UPDATE_SUCCESSFUL :: OrganizationRuleStatus
pattern OrganizationRuleStatus_UPDATE_SUCCESSFUL = OrganizationRuleStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  OrganizationRuleStatus_CREATE_FAILED,
  OrganizationRuleStatus_CREATE_IN_PROGRESS,
  OrganizationRuleStatus_CREATE_SUCCESSFUL,
  OrganizationRuleStatus_DELETE_FAILED,
  OrganizationRuleStatus_DELETE_IN_PROGRESS,
  OrganizationRuleStatus_DELETE_SUCCESSFUL,
  OrganizationRuleStatus_UPDATE_FAILED,
  OrganizationRuleStatus_UPDATE_IN_PROGRESS,
  OrganizationRuleStatus_UPDATE_SUCCESSFUL,
  OrganizationRuleStatus'
  #-}
