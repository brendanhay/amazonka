{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationRuleStatus
  ( OrganizationRuleStatus
    ( OrganizationRuleStatus'
    , OrganizationRuleStatusCreateSuccessful
    , OrganizationRuleStatusCreateInProgress
    , OrganizationRuleStatusCreateFailed
    , OrganizationRuleStatusDeleteSuccessful
    , OrganizationRuleStatusDeleteFailed
    , OrganizationRuleStatusDeleteInProgress
    , OrganizationRuleStatusUpdateSuccessful
    , OrganizationRuleStatusUpdateInProgress
    , OrganizationRuleStatusUpdateFailed
    , fromOrganizationRuleStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OrganizationRuleStatus = OrganizationRuleStatus'{fromOrganizationRuleStatus
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern OrganizationRuleStatusCreateSuccessful :: OrganizationRuleStatus
pattern OrganizationRuleStatusCreateSuccessful = OrganizationRuleStatus' "CREATE_SUCCESSFUL"

pattern OrganizationRuleStatusCreateInProgress :: OrganizationRuleStatus
pattern OrganizationRuleStatusCreateInProgress = OrganizationRuleStatus' "CREATE_IN_PROGRESS"

pattern OrganizationRuleStatusCreateFailed :: OrganizationRuleStatus
pattern OrganizationRuleStatusCreateFailed = OrganizationRuleStatus' "CREATE_FAILED"

pattern OrganizationRuleStatusDeleteSuccessful :: OrganizationRuleStatus
pattern OrganizationRuleStatusDeleteSuccessful = OrganizationRuleStatus' "DELETE_SUCCESSFUL"

pattern OrganizationRuleStatusDeleteFailed :: OrganizationRuleStatus
pattern OrganizationRuleStatusDeleteFailed = OrganizationRuleStatus' "DELETE_FAILED"

pattern OrganizationRuleStatusDeleteInProgress :: OrganizationRuleStatus
pattern OrganizationRuleStatusDeleteInProgress = OrganizationRuleStatus' "DELETE_IN_PROGRESS"

pattern OrganizationRuleStatusUpdateSuccessful :: OrganizationRuleStatus
pattern OrganizationRuleStatusUpdateSuccessful = OrganizationRuleStatus' "UPDATE_SUCCESSFUL"

pattern OrganizationRuleStatusUpdateInProgress :: OrganizationRuleStatus
pattern OrganizationRuleStatusUpdateInProgress = OrganizationRuleStatus' "UPDATE_IN_PROGRESS"

pattern OrganizationRuleStatusUpdateFailed :: OrganizationRuleStatus
pattern OrganizationRuleStatusUpdateFailed = OrganizationRuleStatus' "UPDATE_FAILED"

{-# COMPLETE 
  OrganizationRuleStatusCreateSuccessful,

  OrganizationRuleStatusCreateInProgress,

  OrganizationRuleStatusCreateFailed,

  OrganizationRuleStatusDeleteSuccessful,

  OrganizationRuleStatusDeleteFailed,

  OrganizationRuleStatusDeleteInProgress,

  OrganizationRuleStatusUpdateSuccessful,

  OrganizationRuleStatusUpdateInProgress,

  OrganizationRuleStatusUpdateFailed,
  OrganizationRuleStatus'
  #-}
