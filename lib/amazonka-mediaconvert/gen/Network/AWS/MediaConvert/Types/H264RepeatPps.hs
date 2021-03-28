{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264RepeatPps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264RepeatPps
  ( H264RepeatPps
    ( H264RepeatPps'
    , H264RepeatPpsDisabled
    , H264RepeatPpsEnabled
    , fromH264RepeatPps
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Places a PPS header on each encoded picture, even if repeated.
newtype H264RepeatPps = H264RepeatPps'{fromH264RepeatPps ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern H264RepeatPpsDisabled :: H264RepeatPps
pattern H264RepeatPpsDisabled = H264RepeatPps' "DISABLED"

pattern H264RepeatPpsEnabled :: H264RepeatPps
pattern H264RepeatPpsEnabled = H264RepeatPps' "ENABLED"

{-# COMPLETE 
  H264RepeatPpsDisabled,

  H264RepeatPpsEnabled,
  H264RepeatPps'
  #-}
