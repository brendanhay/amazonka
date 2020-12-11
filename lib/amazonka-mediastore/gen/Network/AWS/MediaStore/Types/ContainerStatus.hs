-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.ContainerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.ContainerStatus
  ( ContainerStatus
      ( ContainerStatus',
        Active,
        Creating,
        Deleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerStatus = ContainerStatus' Lude.Text
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

pattern Active :: ContainerStatus
pattern Active = ContainerStatus' "ACTIVE"

pattern Creating :: ContainerStatus
pattern Creating = ContainerStatus' "CREATING"

pattern Deleting :: ContainerStatus
pattern Deleting = ContainerStatus' "DELETING"

{-# COMPLETE
  Active,
  Creating,
  Deleting,
  ContainerStatus'
  #-}
