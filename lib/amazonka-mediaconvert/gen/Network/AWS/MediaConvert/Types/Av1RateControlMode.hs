{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1RateControlMode
  ( Av1RateControlMode
    ( Av1RateControlMode'
    , Av1RateControlModeQvbr
    , fromAv1RateControlMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
newtype Av1RateControlMode = Av1RateControlMode'{fromAv1RateControlMode
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern Av1RateControlModeQvbr :: Av1RateControlMode
pattern Av1RateControlModeQvbr = Av1RateControlMode' "QVBR"

{-# COMPLETE 
  Av1RateControlModeQvbr,
  Av1RateControlMode'
  #-}
