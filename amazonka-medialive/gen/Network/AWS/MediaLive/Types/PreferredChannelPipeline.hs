{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PreferredChannelPipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PreferredChannelPipeline
  ( PreferredChannelPipeline
      ( ..,
        PreferredChannelPipeline_CURRENTLY_ACTIVE,
        PreferredChannelPipeline_PIPELINE_0,
        PreferredChannelPipeline_PIPELINE_1
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Indicates which pipeline is preferred by the multiplex for program
-- ingest. If set to \\\"PIPELINE_0\\\" or \\\"PIPELINE_1\\\" and an
-- unhealthy ingest causes the multiplex to switch to the non-preferred
-- pipeline, it will switch back once that ingest is healthy again. If set
-- to \\\"CURRENTLY_ACTIVE\\\", it will not switch back to the other
-- pipeline based on it recovering to a healthy state, it will only switch
-- if the active pipeline becomes unhealthy.
newtype PreferredChannelPipeline = PreferredChannelPipeline'
  { fromPreferredChannelPipeline ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PreferredChannelPipeline_CURRENTLY_ACTIVE :: PreferredChannelPipeline
pattern PreferredChannelPipeline_CURRENTLY_ACTIVE = PreferredChannelPipeline' "CURRENTLY_ACTIVE"

pattern PreferredChannelPipeline_PIPELINE_0 :: PreferredChannelPipeline
pattern PreferredChannelPipeline_PIPELINE_0 = PreferredChannelPipeline' "PIPELINE_0"

pattern PreferredChannelPipeline_PIPELINE_1 :: PreferredChannelPipeline
pattern PreferredChannelPipeline_PIPELINE_1 = PreferredChannelPipeline' "PIPELINE_1"

{-# COMPLETE
  PreferredChannelPipeline_CURRENTLY_ACTIVE,
  PreferredChannelPipeline_PIPELINE_0,
  PreferredChannelPipeline_PIPELINE_1,
  PreferredChannelPipeline'
  #-}
