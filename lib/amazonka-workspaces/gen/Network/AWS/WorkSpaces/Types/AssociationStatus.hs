{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AssociationStatusNotAssociated,
        AssociationStatusAssociatedWithOwnerAccount,
        AssociationStatusAssociatedWithSharedAccount,
        AssociationStatusPendingAssociation,
        AssociationStatusPendingDisassociation,
        fromAssociationStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AssociationStatus = AssociationStatus'
  { fromAssociationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AssociationStatusNotAssociated :: AssociationStatus
pattern AssociationStatusNotAssociated = AssociationStatus' "NOT_ASSOCIATED"

pattern AssociationStatusAssociatedWithOwnerAccount :: AssociationStatus
pattern AssociationStatusAssociatedWithOwnerAccount = AssociationStatus' "ASSOCIATED_WITH_OWNER_ACCOUNT"

pattern AssociationStatusAssociatedWithSharedAccount :: AssociationStatus
pattern AssociationStatusAssociatedWithSharedAccount = AssociationStatus' "ASSOCIATED_WITH_SHARED_ACCOUNT"

pattern AssociationStatusPendingAssociation :: AssociationStatus
pattern AssociationStatusPendingAssociation = AssociationStatus' "PENDING_ASSOCIATION"

pattern AssociationStatusPendingDisassociation :: AssociationStatus
pattern AssociationStatusPendingDisassociation = AssociationStatus' "PENDING_DISASSOCIATION"

{-# COMPLETE
  AssociationStatusNotAssociated,
  AssociationStatusAssociatedWithOwnerAccount,
  AssociationStatusAssociatedWithSharedAccount,
  AssociationStatusPendingAssociation,
  AssociationStatusPendingDisassociation,
  AssociationStatus'
  #-}
