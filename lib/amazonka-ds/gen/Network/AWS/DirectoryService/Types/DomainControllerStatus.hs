-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainControllerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainControllerStatus
  ( DomainControllerStatus
      ( DomainControllerStatus',
        Active,
        Creating,
        Deleted,
        Deleting,
        Failed,
        Impaired,
        Restoring
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainControllerStatus = DomainControllerStatus' Lude.Text
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

pattern Active :: DomainControllerStatus
pattern Active = DomainControllerStatus' "Active"

pattern Creating :: DomainControllerStatus
pattern Creating = DomainControllerStatus' "Creating"

pattern Deleted :: DomainControllerStatus
pattern Deleted = DomainControllerStatus' "Deleted"

pattern Deleting :: DomainControllerStatus
pattern Deleting = DomainControllerStatus' "Deleting"

pattern Failed :: DomainControllerStatus
pattern Failed = DomainControllerStatus' "Failed"

pattern Impaired :: DomainControllerStatus
pattern Impaired = DomainControllerStatus' "Impaired"

pattern Restoring :: DomainControllerStatus
pattern Restoring = DomainControllerStatus' "Restoring"

{-# COMPLETE
  Active,
  Creating,
  Deleted,
  Deleting,
  Failed,
  Impaired,
  Restoring,
  DomainControllerStatus'
  #-}
