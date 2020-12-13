{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
  ( DvbSubtitleBackgroundColor
      ( DvbSubtitleBackgroundColor',
        DSBCNone,
        DSBCBlack,
        DSBCWhite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor' Lude.Text
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

pattern DSBCNone :: DvbSubtitleBackgroundColor
pattern DSBCNone = DvbSubtitleBackgroundColor' "NONE"

pattern DSBCBlack :: DvbSubtitleBackgroundColor
pattern DSBCBlack = DvbSubtitleBackgroundColor' "BLACK"

pattern DSBCWhite :: DvbSubtitleBackgroundColor
pattern DSBCWhite = DvbSubtitleBackgroundColor' "WHITE"

{-# COMPLETE
  DSBCNone,
  DSBCBlack,
  DSBCWhite,
  DvbSubtitleBackgroundColor'
  #-}
