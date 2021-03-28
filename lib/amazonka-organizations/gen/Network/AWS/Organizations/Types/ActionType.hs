{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.ActionType
  ( ActionType
    ( ActionType'
    , ActionTypeInvite
    , ActionTypeEnableAllFeatures
    , ActionTypeApproveAllFeatures
    , ActionTypeAddOrganizationsServiceLinkedRole
    , fromActionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ActionType = ActionType'{fromActionType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern ActionTypeInvite :: ActionType
pattern ActionTypeInvite = ActionType' "INVITE"

pattern ActionTypeEnableAllFeatures :: ActionType
pattern ActionTypeEnableAllFeatures = ActionType' "ENABLE_ALL_FEATURES"

pattern ActionTypeApproveAllFeatures :: ActionType
pattern ActionTypeApproveAllFeatures = ActionType' "APPROVE_ALL_FEATURES"

pattern ActionTypeAddOrganizationsServiceLinkedRole :: ActionType
pattern ActionTypeAddOrganizationsServiceLinkedRole = ActionType' "ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE"

{-# COMPLETE 
  ActionTypeInvite,

  ActionTypeEnableAllFeatures,

  ActionTypeApproveAllFeatures,

  ActionTypeAddOrganizationsServiceLinkedRole,
  ActionType'
  #-}
