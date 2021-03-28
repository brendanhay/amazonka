{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
  ( DolbyVisionLevel6Mode
    ( DolbyVisionLevel6Mode'
    , DolbyVisionLevel6ModePassthrough
    , DolbyVisionLevel6ModeRecalculate
    , DolbyVisionLevel6ModeSpecify
    , fromDolbyVisionLevel6Mode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
newtype DolbyVisionLevel6Mode = DolbyVisionLevel6Mode'{fromDolbyVisionLevel6Mode
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern DolbyVisionLevel6ModePassthrough :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6ModePassthrough = DolbyVisionLevel6Mode' "PASSTHROUGH"

pattern DolbyVisionLevel6ModeRecalculate :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6ModeRecalculate = DolbyVisionLevel6Mode' "RECALCULATE"

pattern DolbyVisionLevel6ModeSpecify :: DolbyVisionLevel6Mode
pattern DolbyVisionLevel6ModeSpecify = DolbyVisionLevel6Mode' "SPECIFY"

{-# COMPLETE 
  DolbyVisionLevel6ModePassthrough,

  DolbyVisionLevel6ModeRecalculate,

  DolbyVisionLevel6ModeSpecify,
  DolbyVisionLevel6Mode'
  #-}
