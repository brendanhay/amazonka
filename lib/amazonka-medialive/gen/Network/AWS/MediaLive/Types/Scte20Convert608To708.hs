-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20Convert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte20Convert608To708
  ( Scte20Convert608To708
      ( Scte20Convert608To708',
        SCTDisabled,
        SCTUpconvert
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Scte20 Convert608 To708
newtype Scte20Convert608To708 = Scte20Convert608To708' Lude.Text
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

pattern SCTDisabled :: Scte20Convert608To708
pattern SCTDisabled = Scte20Convert608To708' "DISABLED"

pattern SCTUpconvert :: Scte20Convert608To708
pattern SCTUpconvert = Scte20Convert608To708' "UPCONVERT"

{-# COMPLETE
  SCTDisabled,
  SCTUpconvert,
  Scte20Convert608To708'
  #-}
