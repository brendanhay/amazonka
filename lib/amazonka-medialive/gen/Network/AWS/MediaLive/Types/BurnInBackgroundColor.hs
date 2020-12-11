-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInBackgroundColor
  ( BurnInBackgroundColor
      ( BurnInBackgroundColor',
        BIBCBlack,
        BIBCNone,
        BIBCWhite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Burn In Background Color
newtype BurnInBackgroundColor = BurnInBackgroundColor' Lude.Text
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

pattern BIBCBlack :: BurnInBackgroundColor
pattern BIBCBlack = BurnInBackgroundColor' "BLACK"

pattern BIBCNone :: BurnInBackgroundColor
pattern BIBCNone = BurnInBackgroundColor' "NONE"

pattern BIBCWhite :: BurnInBackgroundColor
pattern BIBCWhite = BurnInBackgroundColor' "WHITE"

{-# COMPLETE
  BIBCBlack,
  BIBCNone,
  BIBCWhite,
  BurnInBackgroundColor'
  #-}
