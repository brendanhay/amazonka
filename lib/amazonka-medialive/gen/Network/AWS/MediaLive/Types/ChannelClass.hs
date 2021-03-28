{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ChannelClass
  ( ChannelClass
    ( ChannelClass'
    , ChannelClassStandard
    , ChannelClassSinglePipeline
    , fromChannelClass
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | A standard channel has two encoding pipelines and a single pipeline channel only has one.
newtype ChannelClass = ChannelClass'{fromChannelClass :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ChannelClassStandard :: ChannelClass
pattern ChannelClassStandard = ChannelClass' "STANDARD"

pattern ChannelClassSinglePipeline :: ChannelClass
pattern ChannelClassSinglePipeline = ChannelClass' "SINGLE_PIPELINE"

{-# COMPLETE 
  ChannelClassStandard,

  ChannelClassSinglePipeline,
  ChannelClass'
  #-}
