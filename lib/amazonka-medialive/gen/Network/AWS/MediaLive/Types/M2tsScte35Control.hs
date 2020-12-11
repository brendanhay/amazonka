-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsScte35Control
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsScte35Control
  ( M2tsScte35Control
      ( M2tsScte35Control',
        MSCNone,
        MSCPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Scte35 Control
newtype M2tsScte35Control = M2tsScte35Control' Lude.Text
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

pattern MSCNone :: M2tsScte35Control
pattern MSCNone = M2tsScte35Control' "NONE"

pattern MSCPassthrough :: M2tsScte35Control
pattern MSCPassthrough = M2tsScte35Control' "PASSTHROUGH"

{-# COMPLETE
  MSCNone,
  MSCPassthrough,
  M2tsScte35Control'
  #-}
