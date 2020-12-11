-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceStatus
  ( OrganizationResourceStatus
      ( OrganizationResourceStatus',
        ORSCreateFailed,
        ORSCreateInProgress,
        ORSCreateSuccessful,
        ORSDeleteFailed,
        ORSDeleteInProgress,
        ORSDeleteSuccessful,
        ORSUpdateFailed,
        ORSUpdateInProgress,
        ORSUpdateSuccessful
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrganizationResourceStatus = OrganizationResourceStatus' Lude.Text
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

pattern ORSCreateFailed :: OrganizationResourceStatus
pattern ORSCreateFailed = OrganizationResourceStatus' "CREATE_FAILED"

pattern ORSCreateInProgress :: OrganizationResourceStatus
pattern ORSCreateInProgress = OrganizationResourceStatus' "CREATE_IN_PROGRESS"

pattern ORSCreateSuccessful :: OrganizationResourceStatus
pattern ORSCreateSuccessful = OrganizationResourceStatus' "CREATE_SUCCESSFUL"

pattern ORSDeleteFailed :: OrganizationResourceStatus
pattern ORSDeleteFailed = OrganizationResourceStatus' "DELETE_FAILED"

pattern ORSDeleteInProgress :: OrganizationResourceStatus
pattern ORSDeleteInProgress = OrganizationResourceStatus' "DELETE_IN_PROGRESS"

pattern ORSDeleteSuccessful :: OrganizationResourceStatus
pattern ORSDeleteSuccessful = OrganizationResourceStatus' "DELETE_SUCCESSFUL"

pattern ORSUpdateFailed :: OrganizationResourceStatus
pattern ORSUpdateFailed = OrganizationResourceStatus' "UPDATE_FAILED"

pattern ORSUpdateInProgress :: OrganizationResourceStatus
pattern ORSUpdateInProgress = OrganizationResourceStatus' "UPDATE_IN_PROGRESS"

pattern ORSUpdateSuccessful :: OrganizationResourceStatus
pattern ORSUpdateSuccessful = OrganizationResourceStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  ORSCreateFailed,
  ORSCreateInProgress,
  ORSCreateSuccessful,
  ORSDeleteFailed,
  ORSDeleteInProgress,
  ORSDeleteSuccessful,
  ORSUpdateFailed,
  ORSUpdateInProgress,
  ORSUpdateSuccessful,
  OrganizationResourceStatus'
  #-}
