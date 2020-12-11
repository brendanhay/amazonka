-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
  ( DedicatedTenancyModificationStateEnum
      ( DedicatedTenancyModificationStateEnum',
        Completed,
        Failed,
        Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DedicatedTenancyModificationStateEnum = DedicatedTenancyModificationStateEnum' Lude.Text
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

pattern Completed :: DedicatedTenancyModificationStateEnum
pattern Completed = DedicatedTenancyModificationStateEnum' "COMPLETED"

pattern Failed :: DedicatedTenancyModificationStateEnum
pattern Failed = DedicatedTenancyModificationStateEnum' "FAILED"

pattern Pending :: DedicatedTenancyModificationStateEnum
pattern Pending = DedicatedTenancyModificationStateEnum' "PENDING"

{-# COMPLETE
  Completed,
  Failed,
  Pending,
  DedicatedTenancyModificationStateEnum'
  #-}
