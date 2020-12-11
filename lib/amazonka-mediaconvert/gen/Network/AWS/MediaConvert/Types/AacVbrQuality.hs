-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacVbrQuality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacVbrQuality
  ( AacVbrQuality
      ( AacVbrQuality',
        AVQHigh,
        AVQLow,
        AVQMediumHigh,
        AVQMediumLow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
newtype AacVbrQuality = AacVbrQuality' Lude.Text
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

pattern AVQHigh :: AacVbrQuality
pattern AVQHigh = AacVbrQuality' "HIGH"

pattern AVQLow :: AacVbrQuality
pattern AVQLow = AacVbrQuality' "LOW"

pattern AVQMediumHigh :: AacVbrQuality
pattern AVQMediumHigh = AacVbrQuality' "MEDIUM_HIGH"

pattern AVQMediumLow :: AacVbrQuality
pattern AVQMediumLow = AacVbrQuality' "MEDIUM_LOW"

{-# COMPLETE
  AVQHigh,
  AVQLow,
  AVQMediumHigh,
  AVQMediumLow,
  AacVbrQuality'
  #-}
