-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillaryConvert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillaryConvert608To708
  ( AncillaryConvert608To708
      ( AncillaryConvert608To708',
        ACTDisabled,
        ACTUpconvert
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
newtype AncillaryConvert608To708 = AncillaryConvert608To708' Lude.Text
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

pattern ACTDisabled :: AncillaryConvert608To708
pattern ACTDisabled = AncillaryConvert608To708' "DISABLED"

pattern ACTUpconvert :: AncillaryConvert608To708
pattern ACTUpconvert = AncillaryConvert608To708' "UPCONVERT"

{-# COMPLETE
  ACTDisabled,
  ACTUpconvert,
  AncillaryConvert608To708'
  #-}
