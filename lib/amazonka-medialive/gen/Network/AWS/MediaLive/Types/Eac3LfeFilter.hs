{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3LfeFilter
  ( Eac3LfeFilter
    ( Eac3LfeFilter'
    , Eac3LfeFilterDisabled
    , Eac3LfeFilterEnabled
    , fromEac3LfeFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Lfe Filter
newtype Eac3LfeFilter = Eac3LfeFilter'{fromEac3LfeFilter ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern Eac3LfeFilterDisabled :: Eac3LfeFilter
pattern Eac3LfeFilterDisabled = Eac3LfeFilter' "DISABLED"

pattern Eac3LfeFilterEnabled :: Eac3LfeFilter
pattern Eac3LfeFilterEnabled = Eac3LfeFilter' "ENABLED"

{-# COMPLETE 
  Eac3LfeFilterDisabled,

  Eac3LfeFilterEnabled,
  Eac3LfeFilter'
  #-}
