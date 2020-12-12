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
        DSTActive,
        DSTCreating,
        DSTDeleting,
        DSTFailed,
        DSTUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainStatusType = DomainStatusType' Lude.Text
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

pattern DSTActive :: DomainStatusType
pattern DSTActive = DomainStatusType' "ACTIVE"

pattern DSTCreating :: DomainStatusType
pattern DSTCreating = DomainStatusType' "CREATING"

pattern DSTDeleting :: DomainStatusType
pattern DSTDeleting = DomainStatusType' "DELETING"

pattern DSTFailed :: DomainStatusType
pattern DSTFailed = DomainStatusType' "FAILED"

pattern DSTUpdating :: DomainStatusType
pattern DSTUpdating = DomainStatusType' "UPDATING"

{-# COMPLETE
  DSTActive,
  DSTCreating,
  DSTDeleting,
  DSTFailed,
  DSTUpdating,
  DomainStatusType'
  #-}
