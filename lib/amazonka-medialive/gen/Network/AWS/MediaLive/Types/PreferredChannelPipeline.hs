{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PreferredChannelPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PreferredChannelPipeline
  ( PreferredChannelPipeline
      ( PreferredChannelPipeline',
        CurrentlyActive,
        Pipeline0,
        Pipeline1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Indicates which pipeline is preferred by the multiplex for program ingest.
--
-- If set to \"PIPELINE_0\" or \"PIPELINE_1\" and an unhealthy ingest causes the multiplex to switch to the non-preferred pipeline,
-- it will switch back once that ingest is healthy again. If set to \"CURRENTLY_ACTIVE\",
-- it will not switch back to the other pipeline based on it recovering to a healthy state,
-- it will only switch if the active pipeline becomes unhealthy.
newtype PreferredChannelPipeline = PreferredChannelPipeline' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CurrentlyActive :: PreferredChannelPipeline
pattern CurrentlyActive = PreferredChannelPipeline' "CURRENTLY_ACTIVE"

pattern Pipeline0 :: PreferredChannelPipeline
pattern Pipeline0 = PreferredChannelPipeline' "PIPELINE_0"

pattern Pipeline1 :: PreferredChannelPipeline
pattern Pipeline1 = PreferredChannelPipeline' "PIPELINE_1"

{-# COMPLETE
  CurrentlyActive,
  Pipeline0,
  Pipeline1,
  PreferredChannelPipeline'
  #-}
