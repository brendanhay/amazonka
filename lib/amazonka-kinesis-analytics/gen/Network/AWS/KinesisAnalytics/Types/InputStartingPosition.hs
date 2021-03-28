{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputStartingPosition
  ( InputStartingPosition
    ( InputStartingPosition'
    , InputStartingPositionNow
    , InputStartingPositionTrimHorizon
    , InputStartingPositionLastStoppedPoint
    , fromInputStartingPosition
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InputStartingPosition = InputStartingPosition'{fromInputStartingPosition
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern InputStartingPositionNow :: InputStartingPosition
pattern InputStartingPositionNow = InputStartingPosition' "NOW"

pattern InputStartingPositionTrimHorizon :: InputStartingPosition
pattern InputStartingPositionTrimHorizon = InputStartingPosition' "TRIM_HORIZON"

pattern InputStartingPositionLastStoppedPoint :: InputStartingPosition
pattern InputStartingPositionLastStoppedPoint = InputStartingPosition' "LAST_STOPPED_POINT"

{-# COMPLETE 
  InputStartingPositionNow,

  InputStartingPositionTrimHorizon,

  InputStartingPositionLastStoppedPoint,
  InputStartingPosition'
  #-}
