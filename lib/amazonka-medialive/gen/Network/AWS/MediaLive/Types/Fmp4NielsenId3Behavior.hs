{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
  ( Fmp4NielsenId3Behavior
    ( Fmp4NielsenId3Behavior'
    , Fmp4NielsenId3BehaviorNoPassthrough
    , Fmp4NielsenId3BehaviorPassthrough
    , fromFmp4NielsenId3Behavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Fmp4 Nielsen Id3 Behavior
newtype Fmp4NielsenId3Behavior = Fmp4NielsenId3Behavior'{fromFmp4NielsenId3Behavior
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern Fmp4NielsenId3BehaviorNoPassthrough :: Fmp4NielsenId3Behavior
pattern Fmp4NielsenId3BehaviorNoPassthrough = Fmp4NielsenId3Behavior' "NO_PASSTHROUGH"

pattern Fmp4NielsenId3BehaviorPassthrough :: Fmp4NielsenId3Behavior
pattern Fmp4NielsenId3BehaviorPassthrough = Fmp4NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE 
  Fmp4NielsenId3BehaviorNoPassthrough,

  Fmp4NielsenId3BehaviorPassthrough,
  Fmp4NielsenId3Behavior'
  #-}
