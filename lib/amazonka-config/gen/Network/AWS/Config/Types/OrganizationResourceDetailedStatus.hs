{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceDetailedStatus
  ( OrganizationResourceDetailedStatus
      ( OrganizationResourceDetailedStatus',
        CreateSuccessful,
        CreateInProgress,
        CreateFailed,
        DeleteSuccessful,
        DeleteFailed,
        DeleteInProgress,
        UpdateSuccessful,
        UpdateInProgress,
        UpdateFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrganizationResourceDetailedStatus = OrganizationResourceDetailedStatus' Lude.Text
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

pattern CreateSuccessful :: OrganizationResourceDetailedStatus
pattern CreateSuccessful = OrganizationResourceDetailedStatus' "CREATE_SUCCESSFUL"

pattern CreateInProgress :: OrganizationResourceDetailedStatus
pattern CreateInProgress = OrganizationResourceDetailedStatus' "CREATE_IN_PROGRESS"

pattern CreateFailed :: OrganizationResourceDetailedStatus
pattern CreateFailed = OrganizationResourceDetailedStatus' "CREATE_FAILED"

pattern DeleteSuccessful :: OrganizationResourceDetailedStatus
pattern DeleteSuccessful = OrganizationResourceDetailedStatus' "DELETE_SUCCESSFUL"

pattern DeleteFailed :: OrganizationResourceDetailedStatus
pattern DeleteFailed = OrganizationResourceDetailedStatus' "DELETE_FAILED"

pattern DeleteInProgress :: OrganizationResourceDetailedStatus
pattern DeleteInProgress = OrganizationResourceDetailedStatus' "DELETE_IN_PROGRESS"

pattern UpdateSuccessful :: OrganizationResourceDetailedStatus
pattern UpdateSuccessful = OrganizationResourceDetailedStatus' "UPDATE_SUCCESSFUL"

pattern UpdateInProgress :: OrganizationResourceDetailedStatus
pattern UpdateInProgress = OrganizationResourceDetailedStatus' "UPDATE_IN_PROGRESS"

pattern UpdateFailed :: OrganizationResourceDetailedStatus
pattern UpdateFailed = OrganizationResourceDetailedStatus' "UPDATE_FAILED"

{-# COMPLETE
  CreateSuccessful,
  CreateInProgress,
  CreateFailed,
  DeleteSuccessful,
  DeleteFailed,
  DeleteInProgress,
  UpdateSuccessful,
  UpdateInProgress,
  UpdateFailed,
  OrganizationResourceDetailedStatus'
  #-}
