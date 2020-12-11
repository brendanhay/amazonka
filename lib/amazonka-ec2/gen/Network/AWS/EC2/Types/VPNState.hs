-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPNState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPNState
  ( VPNState
      ( VPNState',
        VSAvailable,
        VSDeleted,
        VSDeleting,
        VSPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VPNState = VPNState' Lude.Text
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

pattern VSAvailable :: VPNState
pattern VSAvailable = VPNState' "available"

pattern VSDeleted :: VPNState
pattern VSDeleted = VPNState' "deleted"

pattern VSDeleting :: VPNState
pattern VSDeleting = VPNState' "deleting"

pattern VSPending :: VPNState
pattern VSPending = VPNState' "pending"

{-# COMPLETE
  VSAvailable,
  VSDeleted,
  VSDeleting,
  VSPending,
  VPNState'
  #-}
