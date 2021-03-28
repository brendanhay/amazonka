{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Ac3LfeFilter
  ( Ac3LfeFilter
    ( Ac3LfeFilter'
    , Ac3LfeFilterEnabled
    , Ac3LfeFilterDisabled
    , fromAc3LfeFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Ac3LfeFilter = Ac3LfeFilter'{fromAc3LfeFilter :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern Ac3LfeFilterEnabled :: Ac3LfeFilter
pattern Ac3LfeFilterEnabled = Ac3LfeFilter' "ENABLED"

pattern Ac3LfeFilterDisabled :: Ac3LfeFilter
pattern Ac3LfeFilterDisabled = Ac3LfeFilter' "DISABLED"

{-# COMPLETE 
  Ac3LfeFilterEnabled,

  Ac3LfeFilterDisabled,
  Ac3LfeFilter'
  #-}
