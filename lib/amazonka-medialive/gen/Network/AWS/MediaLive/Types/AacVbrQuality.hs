{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacVbrQuality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacVbrQuality
  ( AacVbrQuality
      ( AacVbrQuality',
        AacVbrQualityHigh,
        AacVbrQualityLow,
        AacVbrQualityMediumHigh,
        AacVbrQualityMediumLow,
        fromAacVbrQuality
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Aac Vbr Quality
newtype AacVbrQuality = AacVbrQuality'
  { fromAacVbrQuality ::
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

pattern AacVbrQualityHigh :: AacVbrQuality
pattern AacVbrQualityHigh = AacVbrQuality' "HIGH"

pattern AacVbrQualityLow :: AacVbrQuality
pattern AacVbrQualityLow = AacVbrQuality' "LOW"

pattern AacVbrQualityMediumHigh :: AacVbrQuality
pattern AacVbrQualityMediumHigh = AacVbrQuality' "MEDIUM_HIGH"

pattern AacVbrQualityMediumLow :: AacVbrQuality
pattern AacVbrQualityMediumLow = AacVbrQuality' "MEDIUM_LOW"

{-# COMPLETE
  AacVbrQualityHigh,
  AacVbrQualityLow,
  AacVbrQualityMediumHigh,
  AacVbrQualityMediumLow,
  AacVbrQuality'
  #-}
