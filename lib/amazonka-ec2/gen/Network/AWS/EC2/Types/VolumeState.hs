{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeState
  ( VolumeState
      ( VolumeState',
        VCreating,
        VAvailable,
        VInUse,
        VDeleting,
        VDeleted,
        VError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VolumeState = VolumeState' Lude.Text
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

pattern VCreating :: VolumeState
pattern VCreating = VolumeState' "creating"

pattern VAvailable :: VolumeState
pattern VAvailable = VolumeState' "available"

pattern VInUse :: VolumeState
pattern VInUse = VolumeState' "in-use"

pattern VDeleting :: VolumeState
pattern VDeleting = VolumeState' "deleting"

pattern VDeleted :: VolumeState
pattern VDeleted = VolumeState' "deleted"

pattern VError :: VolumeState
pattern VError = VolumeState' "error"

{-# COMPLETE
  VCreating,
  VAvailable,
  VInUse,
  VDeleting,
  VDeleted,
  VError,
  VolumeState'
  #-}
