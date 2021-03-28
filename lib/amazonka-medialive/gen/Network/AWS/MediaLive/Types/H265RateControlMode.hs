{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265RateControlMode
  ( H265RateControlMode
    ( H265RateControlMode'
    , H265RateControlModeCbr
    , H265RateControlModeMultiplex
    , H265RateControlModeQvbr
    , fromH265RateControlMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H265 Rate Control Mode
newtype H265RateControlMode = H265RateControlMode'{fromH265RateControlMode
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern H265RateControlModeCbr :: H265RateControlMode
pattern H265RateControlModeCbr = H265RateControlMode' "CBR"

pattern H265RateControlModeMultiplex :: H265RateControlMode
pattern H265RateControlModeMultiplex = H265RateControlMode' "MULTIPLEX"

pattern H265RateControlModeQvbr :: H265RateControlMode
pattern H265RateControlModeQvbr = H265RateControlMode' "QVBR"

{-# COMPLETE 
  H265RateControlModeCbr,

  H265RateControlModeMultiplex,

  H265RateControlModeQvbr,
  H265RateControlMode'
  #-}
