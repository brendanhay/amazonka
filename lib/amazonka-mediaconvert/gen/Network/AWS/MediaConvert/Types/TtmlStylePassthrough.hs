-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TtmlStylePassthrough
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlStylePassthrough
  ( TtmlStylePassthrough
      ( TtmlStylePassthrough',
        Disabled,
        Enabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
newtype TtmlStylePassthrough = TtmlStylePassthrough' Lude.Text
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

pattern Disabled :: TtmlStylePassthrough
pattern Disabled = TtmlStylePassthrough' "DISABLED"

pattern Enabled :: TtmlStylePassthrough
pattern Enabled = TtmlStylePassthrough' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  TtmlStylePassthrough'
  #-}
