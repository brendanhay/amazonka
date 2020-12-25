{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.WatermarkingStrength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WatermarkingStrength
  ( WatermarkingStrength
      ( WatermarkingStrength',
        WatermarkingStrengthLightest,
        WatermarkingStrengthLighter,
        WatermarkingStrengthDefault,
        WatermarkingStrengthStronger,
        WatermarkingStrengthStrongest,
        fromWatermarkingStrength
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
newtype WatermarkingStrength = WatermarkingStrength'
  { fromWatermarkingStrength ::
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

pattern WatermarkingStrengthLightest :: WatermarkingStrength
pattern WatermarkingStrengthLightest = WatermarkingStrength' "LIGHTEST"

pattern WatermarkingStrengthLighter :: WatermarkingStrength
pattern WatermarkingStrengthLighter = WatermarkingStrength' "LIGHTER"

pattern WatermarkingStrengthDefault :: WatermarkingStrength
pattern WatermarkingStrengthDefault = WatermarkingStrength' "DEFAULT"

pattern WatermarkingStrengthStronger :: WatermarkingStrength
pattern WatermarkingStrengthStronger = WatermarkingStrength' "STRONGER"

pattern WatermarkingStrengthStrongest :: WatermarkingStrength
pattern WatermarkingStrengthStrongest = WatermarkingStrength' "STRONGEST"

{-# COMPLETE
  WatermarkingStrengthLightest,
  WatermarkingStrengthLighter,
  WatermarkingStrengthDefault,
  WatermarkingStrengthStronger,
  WatermarkingStrengthStrongest,
  WatermarkingStrength'
  #-}
