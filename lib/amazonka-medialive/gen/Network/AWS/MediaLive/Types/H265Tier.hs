-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Tier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Tier
  ( H265Tier
      ( H265Tier',
        HTHigh,
        HTMain
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Tier
newtype H265Tier = H265Tier' Lude.Text
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

pattern HTHigh :: H265Tier
pattern HTHigh = H265Tier' "HIGH"

pattern HTMain :: H265Tier
pattern HTMain = H265Tier' "MAIN"

{-# COMPLETE
  HTHigh,
  HTMain,
  H265Tier'
  #-}
