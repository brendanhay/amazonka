{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationRuleStatus
  ( OrganizationRuleStatus
      ( OrganizationRuleStatus',
        OCreateFailed,
        OCreateInProgress,
        OCreateSuccessful,
        ODeleteFailed,
        ODeleteInProgress,
        ODeleteSuccessful,
        OUpdateFailed,
        OUpdateInProgress,
        OUpdateSuccessful
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrganizationRuleStatus = OrganizationRuleStatus' Lude.Text
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

pattern OCreateFailed :: OrganizationRuleStatus
pattern OCreateFailed = OrganizationRuleStatus' "CREATE_FAILED"

pattern OCreateInProgress :: OrganizationRuleStatus
pattern OCreateInProgress = OrganizationRuleStatus' "CREATE_IN_PROGRESS"

pattern OCreateSuccessful :: OrganizationRuleStatus
pattern OCreateSuccessful = OrganizationRuleStatus' "CREATE_SUCCESSFUL"

pattern ODeleteFailed :: OrganizationRuleStatus
pattern ODeleteFailed = OrganizationRuleStatus' "DELETE_FAILED"

pattern ODeleteInProgress :: OrganizationRuleStatus
pattern ODeleteInProgress = OrganizationRuleStatus' "DELETE_IN_PROGRESS"

pattern ODeleteSuccessful :: OrganizationRuleStatus
pattern ODeleteSuccessful = OrganizationRuleStatus' "DELETE_SUCCESSFUL"

pattern OUpdateFailed :: OrganizationRuleStatus
pattern OUpdateFailed = OrganizationRuleStatus' "UPDATE_FAILED"

pattern OUpdateInProgress :: OrganizationRuleStatus
pattern OUpdateInProgress = OrganizationRuleStatus' "UPDATE_IN_PROGRESS"

pattern OUpdateSuccessful :: OrganizationRuleStatus
pattern OUpdateSuccessful = OrganizationRuleStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  OCreateFailed,
  OCreateInProgress,
  OCreateSuccessful,
  ODeleteFailed,
  ODeleteInProgress,
  ODeleteSuccessful,
  OUpdateFailed,
  OUpdateInProgress,
  OUpdateSuccessful,
  OrganizationRuleStatus'
  #-}
