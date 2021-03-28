{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputDenoiseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputDenoiseFilter
  ( InputDenoiseFilter
    ( InputDenoiseFilter'
    , InputDenoiseFilterEnabled
    , InputDenoiseFilterDisabled
    , fromInputDenoiseFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
newtype InputDenoiseFilter = InputDenoiseFilter'{fromInputDenoiseFilter
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern InputDenoiseFilterEnabled :: InputDenoiseFilter
pattern InputDenoiseFilterEnabled = InputDenoiseFilter' "ENABLED"

pattern InputDenoiseFilterDisabled :: InputDenoiseFilter
pattern InputDenoiseFilterDisabled = InputDenoiseFilter' "DISABLED"

{-# COMPLETE 
  InputDenoiseFilterEnabled,

  InputDenoiseFilterDisabled,
  InputDenoiseFilter'
  #-}
