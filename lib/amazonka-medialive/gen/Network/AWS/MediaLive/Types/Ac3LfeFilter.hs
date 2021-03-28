{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Ac3LfeFilter
  ( Ac3LfeFilter
    ( Ac3LfeFilter'
    , Ac3LfeFilterDisabled
    , Ac3LfeFilterEnabled
    , fromAc3LfeFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Ac3 Lfe Filter
newtype Ac3LfeFilter = Ac3LfeFilter'{fromAc3LfeFilter :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern Ac3LfeFilterDisabled :: Ac3LfeFilter
pattern Ac3LfeFilterDisabled = Ac3LfeFilter' "DISABLED"

pattern Ac3LfeFilterEnabled :: Ac3LfeFilter
pattern Ac3LfeFilterEnabled = Ac3LfeFilter' "ENABLED"

{-# COMPLETE 
  Ac3LfeFilterDisabled,

  Ac3LfeFilterEnabled,
  Ac3LfeFilter'
  #-}
