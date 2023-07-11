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
-- Module      : Amazonka.MediaLive.Types.PreferredChannelPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.PreferredChannelPipeline
  ( PreferredChannelPipeline
      ( ..,
        PreferredChannelPipeline_CURRENTLY_ACTIVE,
        PreferredChannelPipeline_PIPELINE_0,
        PreferredChannelPipeline_PIPELINE_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates which pipeline is preferred by the multiplex for program
-- ingest. If set to \\\"PIPELINE_0\\\" or \\\"PIPELINE_1\\\" and an
-- unhealthy ingest causes the multiplex to switch to the non-preferred
-- pipeline, it will switch back once that ingest is healthy again. If set
-- to \\\"CURRENTLY_ACTIVE\\\", it will not switch back to the other
-- pipeline based on it recovering to a healthy state, it will only switch
-- if the active pipeline becomes unhealthy.
newtype PreferredChannelPipeline = PreferredChannelPipeline'
  { fromPreferredChannelPipeline ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
