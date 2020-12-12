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
        DSAvailable,
        DSError,
        DSInUse,
        DSPending,
        DSUnknown
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

pattern DSAvailable :: DiskState
pattern DSAvailable = DiskState' "available"

pattern DSError :: DiskState
pattern DSError = DiskState' "error"

pattern DSInUse :: DiskState
pattern DSInUse = DiskState' "in-use"

pattern DSPending :: DiskState
pattern DSPending = DiskState' "pending"

pattern DSUnknown :: DiskState
pattern DSUnknown = DiskState' "unknown"

{-# COMPLETE
  DSAvailable,
  DSError,
  DSInUse,
  DSPending,
  DSUnknown,
  DiskState'
  #-}
