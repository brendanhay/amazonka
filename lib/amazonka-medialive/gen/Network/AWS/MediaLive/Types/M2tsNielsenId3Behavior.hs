{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsNielsenId3Behavior
  ( M2tsNielsenId3Behavior
    ( M2tsNielsenId3Behavior'
    , M2tsNielsenId3BehaviorNoPassthrough
    , M2tsNielsenId3BehaviorPassthrough
    , fromM2tsNielsenId3Behavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M2ts Nielsen Id3 Behavior
newtype M2tsNielsenId3Behavior = M2tsNielsenId3Behavior'{fromM2tsNielsenId3Behavior
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern M2tsNielsenId3BehaviorNoPassthrough :: M2tsNielsenId3Behavior
pattern M2tsNielsenId3BehaviorNoPassthrough = M2tsNielsenId3Behavior' "NO_PASSTHROUGH"

pattern M2tsNielsenId3BehaviorPassthrough :: M2tsNielsenId3Behavior
pattern M2tsNielsenId3BehaviorPassthrough = M2tsNielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE 
  M2tsNielsenId3BehaviorNoPassthrough,

  M2tsNielsenId3BehaviorPassthrough,
  M2tsNielsenId3Behavior'
  #-}
