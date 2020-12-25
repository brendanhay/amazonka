{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
  ( DomainStatusType
      ( DomainStatusType',
        DomainStatusTypeCreating,
        DomainStatusTypeDeleting,
        DomainStatusTypeUpdating,
        DomainStatusTypeActive,
        DomainStatusTypeFailed,
        fromDomainStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DomainStatusType = DomainStatusType'
  { fromDomainStatusType ::
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

pattern DomainStatusTypeCreating :: DomainStatusType
pattern DomainStatusTypeCreating = DomainStatusType' "CREATING"

pattern DomainStatusTypeDeleting :: DomainStatusType
pattern DomainStatusTypeDeleting = DomainStatusType' "DELETING"

pattern DomainStatusTypeUpdating :: DomainStatusType
pattern DomainStatusTypeUpdating = DomainStatusType' "UPDATING"

pattern DomainStatusTypeActive :: DomainStatusType
pattern DomainStatusTypeActive = DomainStatusType' "ACTIVE"

pattern DomainStatusTypeFailed :: DomainStatusType
pattern DomainStatusTypeFailed = DomainStatusType' "FAILED"

{-# COMPLETE
  DomainStatusTypeCreating,
  DomainStatusTypeDeleting,
  DomainStatusTypeUpdating,
  DomainStatusTypeActive,
  DomainStatusTypeFailed,
  DomainStatusType'
  #-}
