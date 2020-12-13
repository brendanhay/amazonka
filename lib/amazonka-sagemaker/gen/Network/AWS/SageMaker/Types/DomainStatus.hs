{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainStatus
  ( DomainStatus
      ( DomainStatus',
        Deleting,
        Failed,
        InService,
        Pending,
        Updating,
        UpdateFailed,
        DeleteFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainStatus = DomainStatus' Lude.Text
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

pattern Deleting :: DomainStatus
pattern Deleting = DomainStatus' "Deleting"

pattern Failed :: DomainStatus
pattern Failed = DomainStatus' "Failed"

pattern InService :: DomainStatus
pattern InService = DomainStatus' "InService"

pattern Pending :: DomainStatus
pattern Pending = DomainStatus' "Pending"

pattern Updating :: DomainStatus
pattern Updating = DomainStatus' "Updating"

pattern UpdateFailed :: DomainStatus
pattern UpdateFailed = DomainStatus' "Update_Failed"

pattern DeleteFailed :: DomainStatus
pattern DeleteFailed = DomainStatus' "Delete_Failed"

{-# COMPLETE
  Deleting,
  Failed,
  InService,
  Pending,
  Updating,
  UpdateFailed,
  DeleteFailed,
  DomainStatus'
  #-}
