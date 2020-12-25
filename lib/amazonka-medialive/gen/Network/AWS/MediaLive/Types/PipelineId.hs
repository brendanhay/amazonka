{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PipelineId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelineId
  ( PipelineId
      ( PipelineId',
        PipelineIdPipeline0,
        PipelineIdPipeline1,
        fromPipelineId
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Pipeline ID
newtype PipelineId = PipelineId' {fromPipelineId :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PipelineIdPipeline0 :: PipelineId
pattern PipelineIdPipeline0 = PipelineId' "PIPELINE_0"

pattern PipelineIdPipeline1 :: PipelineId
pattern PipelineIdPipeline1 = PipelineId' "PIPELINE_1"

{-# COMPLETE
  PipelineIdPipeline0,
  PipelineIdPipeline1,
  PipelineId'
  #-}
