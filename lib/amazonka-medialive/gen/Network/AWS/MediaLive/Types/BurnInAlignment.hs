-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInAlignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInAlignment
  ( BurnInAlignment
      ( BurnInAlignment',
        BIACentered,
        BIALeft,
        BIASmart
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Burn In Alignment
newtype BurnInAlignment = BurnInAlignment' Lude.Text
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

pattern BIACentered :: BurnInAlignment
pattern BIACentered = BurnInAlignment' "CENTERED"

pattern BIALeft :: BurnInAlignment
pattern BIALeft = BurnInAlignment' "LEFT"

pattern BIASmart :: BurnInAlignment
pattern BIASmart = BurnInAlignment' "SMART"

{-# COMPLETE
  BIACentered,
  BIALeft,
  BIASmart,
  BurnInAlignment'
  #-}
