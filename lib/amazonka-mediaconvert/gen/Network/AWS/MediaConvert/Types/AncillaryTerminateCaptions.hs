{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AncillaryTerminateCaptionsEndOfInput,
        AncillaryTerminateCaptionsDisabled,
        fromAncillaryTerminateCaptions
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
newtype AncillaryTerminateCaptions = AncillaryTerminateCaptions'
  { fromAncillaryTerminateCaptions ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AncillaryTerminateCaptionsEndOfInput :: AncillaryTerminateCaptions
pattern AncillaryTerminateCaptionsEndOfInput = AncillaryTerminateCaptions' "END_OF_INPUT"

pattern AncillaryTerminateCaptionsDisabled :: AncillaryTerminateCaptions
pattern AncillaryTerminateCaptionsDisabled = AncillaryTerminateCaptions' "DISABLED"

{-# COMPLETE
  AncillaryTerminateCaptionsEndOfInput,
  AncillaryTerminateCaptionsDisabled,
  AncillaryTerminateCaptions'
  #-}
