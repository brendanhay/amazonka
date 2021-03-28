{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Mp2CodingMode
  ( Mp2CodingMode
    ( Mp2CodingMode'
    , Mp2CodingModeCodingMode10
    , Mp2CodingModeCodingMode20
    , fromMp2CodingMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Mp2 Coding Mode
newtype Mp2CodingMode = Mp2CodingMode'{fromMp2CodingMode ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern Mp2CodingModeCodingMode10 :: Mp2CodingMode
pattern Mp2CodingModeCodingMode10 = Mp2CodingMode' "CODING_MODE_1_0"

pattern Mp2CodingModeCodingMode20 :: Mp2CodingMode
pattern Mp2CodingModeCodingMode20 = Mp2CodingMode' "CODING_MODE_2_0"

{-# COMPLETE 
  Mp2CodingModeCodingMode10,

  Mp2CodingModeCodingMode20,
  Mp2CodingMode'
  #-}
