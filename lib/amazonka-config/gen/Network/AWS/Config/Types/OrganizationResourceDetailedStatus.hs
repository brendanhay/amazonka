{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationResourceDetailedStatus
  ( OrganizationResourceDetailedStatus
    ( OrganizationResourceDetailedStatus'
    , OrganizationResourceDetailedStatusCreateSuccessful
    , OrganizationResourceDetailedStatusCreateInProgress
    , OrganizationResourceDetailedStatusCreateFailed
    , OrganizationResourceDetailedStatusDeleteSuccessful
    , OrganizationResourceDetailedStatusDeleteFailed
    , OrganizationResourceDetailedStatusDeleteInProgress
    , OrganizationResourceDetailedStatusUpdateSuccessful
    , OrganizationResourceDetailedStatusUpdateInProgress
    , OrganizationResourceDetailedStatusUpdateFailed
    , fromOrganizationResourceDetailedStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OrganizationResourceDetailedStatus = OrganizationResourceDetailedStatus'{fromOrganizationResourceDetailedStatus
                                                                                 :: Core.Text}
                                               deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                               Core.Show, Core.Generic)
                                               deriving newtype (Core.IsString, Core.Hashable,
                                                                 Core.NFData, Core.ToJSONKey,
                                                                 Core.FromJSONKey, Core.ToJSON,
                                                                 Core.FromJSON, Core.ToXML,
                                                                 Core.FromXML, Core.ToText,
                                                                 Core.FromText, Core.ToByteString,
                                                                 Core.ToQuery, Core.ToHeader)

pattern OrganizationResourceDetailedStatusCreateSuccessful :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusCreateSuccessful = OrganizationResourceDetailedStatus' "CREATE_SUCCESSFUL"

pattern OrganizationResourceDetailedStatusCreateInProgress :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusCreateInProgress = OrganizationResourceDetailedStatus' "CREATE_IN_PROGRESS"

pattern OrganizationResourceDetailedStatusCreateFailed :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusCreateFailed = OrganizationResourceDetailedStatus' "CREATE_FAILED"

pattern OrganizationResourceDetailedStatusDeleteSuccessful :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusDeleteSuccessful = OrganizationResourceDetailedStatus' "DELETE_SUCCESSFUL"

pattern OrganizationResourceDetailedStatusDeleteFailed :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusDeleteFailed = OrganizationResourceDetailedStatus' "DELETE_FAILED"

pattern OrganizationResourceDetailedStatusDeleteInProgress :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusDeleteInProgress = OrganizationResourceDetailedStatus' "DELETE_IN_PROGRESS"

pattern OrganizationResourceDetailedStatusUpdateSuccessful :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusUpdateSuccessful = OrganizationResourceDetailedStatus' "UPDATE_SUCCESSFUL"

pattern OrganizationResourceDetailedStatusUpdateInProgress :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusUpdateInProgress = OrganizationResourceDetailedStatus' "UPDATE_IN_PROGRESS"

pattern OrganizationResourceDetailedStatusUpdateFailed :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatusUpdateFailed = OrganizationResourceDetailedStatus' "UPDATE_FAILED"

{-# COMPLETE 
  OrganizationResourceDetailedStatusCreateSuccessful,

  OrganizationResourceDetailedStatusCreateInProgress,

  OrganizationResourceDetailedStatusCreateFailed,

  OrganizationResourceDetailedStatusDeleteSuccessful,

  OrganizationResourceDetailedStatusDeleteFailed,

  OrganizationResourceDetailedStatusDeleteInProgress,

  OrganizationResourceDetailedStatusUpdateSuccessful,

  OrganizationResourceDetailedStatusUpdateInProgress,

  OrganizationResourceDetailedStatusUpdateFailed,
  OrganizationResourceDetailedStatus'
  #-}
