{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
  ( DvbSubtitleShadowColor
      ( DvbSubtitleShadowColor',
        DSSCBlack,
        DSSCNone,
        DSSCWhite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleShadowColor = DvbSubtitleShadowColor' Lude.Text
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

pattern DSSCBlack :: DvbSubtitleShadowColor
pattern DSSCBlack = DvbSubtitleShadowColor' "BLACK"

pattern DSSCNone :: DvbSubtitleShadowColor
pattern DSSCNone = DvbSubtitleShadowColor' "NONE"

pattern DSSCWhite :: DvbSubtitleShadowColor
pattern DSSCWhite = DvbSubtitleShadowColor' "WHITE"

{-# COMPLETE
  DSSCBlack,
  DSSCNone,
  DSSCWhite,
  DvbSubtitleShadowColor'
  #-}
