{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.ActionType
  ( ActionType
      ( ActionType',
        AddOrganizationsServiceLinkedRole,
        ApproveAllFeatures,
        EnableAllFeatures,
        Invite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionType = ActionType' Lude.Text
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

pattern AddOrganizationsServiceLinkedRole :: ActionType
pattern AddOrganizationsServiceLinkedRole = ActionType' "ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE"

pattern ApproveAllFeatures :: ActionType
pattern ApproveAllFeatures = ActionType' "APPROVE_ALL_FEATURES"

pattern EnableAllFeatures :: ActionType
pattern EnableAllFeatures = ActionType' "ENABLE_ALL_FEATURES"

pattern Invite :: ActionType
pattern Invite = ActionType' "INVITE"

{-# COMPLETE
  AddOrganizationsServiceLinkedRole,
  ApproveAllFeatures,
  EnableAllFeatures,
  Invite,
  ActionType'
  #-}
