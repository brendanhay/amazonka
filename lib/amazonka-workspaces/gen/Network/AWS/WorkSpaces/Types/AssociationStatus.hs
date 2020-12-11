-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AssociationStatus
  ( AssociationStatus
      ( AssociationStatus',
        AssociatedWithOwnerAccount,
        AssociatedWithSharedAccount,
        NotAssociated,
        PendingAssociation,
        PendingDisassociation
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AssociationStatus = AssociationStatus' Lude.Text
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

pattern AssociatedWithOwnerAccount :: AssociationStatus
pattern AssociatedWithOwnerAccount = AssociationStatus' "ASSOCIATED_WITH_OWNER_ACCOUNT"

pattern AssociatedWithSharedAccount :: AssociationStatus
pattern AssociatedWithSharedAccount = AssociationStatus' "ASSOCIATED_WITH_SHARED_ACCOUNT"

pattern NotAssociated :: AssociationStatus
pattern NotAssociated = AssociationStatus' "NOT_ASSOCIATED"

pattern PendingAssociation :: AssociationStatus
pattern PendingAssociation = AssociationStatus' "PENDING_ASSOCIATION"

pattern PendingDisassociation :: AssociationStatus
pattern PendingDisassociation = AssociationStatus' "PENDING_DISASSOCIATION"

{-# COMPLETE
  AssociatedWithOwnerAccount,
  AssociatedWithSharedAccount,
  NotAssociated,
  PendingAssociation,
  PendingDisassociation,
  AssociationStatus'
  #-}
