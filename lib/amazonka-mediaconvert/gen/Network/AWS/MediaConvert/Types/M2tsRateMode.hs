{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsRateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsRateMode
  ( M2tsRateMode
    ( M2tsRateMode'
    , M2tsRateModeVbr
    , M2tsRateModeCbr
    , fromM2tsRateMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
newtype M2tsRateMode = M2tsRateMode'{fromM2tsRateMode :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern M2tsRateModeVbr :: M2tsRateMode
pattern M2tsRateModeVbr = M2tsRateMode' "VBR"

pattern M2tsRateModeCbr :: M2tsRateMode
pattern M2tsRateModeCbr = M2tsRateMode' "CBR"

{-# COMPLETE 
  M2tsRateModeVbr,

  M2tsRateModeCbr,
  M2tsRateMode'
  #-}
