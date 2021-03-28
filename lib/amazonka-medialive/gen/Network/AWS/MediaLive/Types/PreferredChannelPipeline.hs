{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PreferredChannelPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.PreferredChannelPipeline
  ( PreferredChannelPipeline
    ( PreferredChannelPipeline'
    , PreferredChannelPipelineCurrentlyActive
    , PreferredChannelPipelinePipeline0
    , PreferredChannelPipelinePipeline1
    , fromPreferredChannelPipeline
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- If set to \"PIPELINE_0\" or \"PIPELINE_1\" and an unhealthy ingest causes the multiplex to switch to the non-preferred pipeline,
-- it will switch back once that ingest is healthy again. If set to \"CURRENTLY_ACTIVE\",
-- it will not switch back to the other pipeline based on it recovering to a healthy state,
-- it will only switch if the active pipeline becomes unhealthy.
newtype PreferredChannelPipeline = PreferredChannelPipeline'{fromPreferredChannelPipeline
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern PreferredChannelPipelineCurrentlyActive :: PreferredChannelPipeline
pattern PreferredChannelPipelineCurrentlyActive = PreferredChannelPipeline' "CURRENTLY_ACTIVE"

pattern PreferredChannelPipelinePipeline0 :: PreferredChannelPipeline
pattern PreferredChannelPipelinePipeline0 = PreferredChannelPipeline' "PIPELINE_0"

pattern PreferredChannelPipelinePipeline1 :: PreferredChannelPipeline
pattern PreferredChannelPipelinePipeline1 = PreferredChannelPipeline' "PIPELINE_1"

{-# COMPLETE 
  PreferredChannelPipelineCurrentlyActive,

  PreferredChannelPipelinePipeline0,

  PreferredChannelPipelinePipeline1,
  PreferredChannelPipeline'
  #-}
