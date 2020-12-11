-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
  ( AncillaryTerminateCaptions
      ( AncillaryTerminateCaptions',
        ATCDisabled,
        ATCEndOfInput
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
newtype AncillaryTerminateCaptions = AncillaryTerminateCaptions' Lude.Text
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

pattern ATCDisabled :: AncillaryTerminateCaptions
pattern ATCDisabled = AncillaryTerminateCaptions' "DISABLED"

pattern ATCEndOfInput :: AncillaryTerminateCaptions
pattern ATCEndOfInput = AncillaryTerminateCaptions' "END_OF_INPUT"

{-# COMPLETE
  ATCDisabled,
  ATCEndOfInput,
  AncillaryTerminateCaptions'
  #-}
