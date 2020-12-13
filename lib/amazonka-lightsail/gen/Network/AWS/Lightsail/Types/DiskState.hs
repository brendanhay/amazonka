{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskState
  ( DiskState
      ( DiskState',
        Pending,
        Error,
        Available,
        InUse,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DiskState = DiskState' Lude.Text
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

pattern Pending :: DiskState
pattern Pending = DiskState' "pending"

pattern Error :: DiskState
pattern Error = DiskState' "error"

pattern Available :: DiskState
pattern Available = DiskState' "available"

pattern InUse :: DiskState
pattern InUse = DiskState' "in-use"

pattern Unknown :: DiskState
pattern Unknown = DiskState' "unknown"

{-# COMPLETE
  Pending,
  Error,
  Available,
  InUse,
  Unknown,
  DiskState'
  #-}
