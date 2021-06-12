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
-- Module      : Network.AWS.MediaLive.Types.PipelineId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelineId
  ( PipelineId
      ( ..,
        PipelineId_PIPELINE_0,
        PipelineId_PIPELINE_1
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Pipeline ID
newtype PipelineId = PipelineId'
  { fromPipelineId ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern PipelineId_PIPELINE_0 :: PipelineId
pattern PipelineId_PIPELINE_0 = PipelineId' "PIPELINE_0"

pattern PipelineId_PIPELINE_1 :: PipelineId
pattern PipelineId_PIPELINE_1 = PipelineId' "PIPELINE_1"

{-# COMPLETE
  PipelineId_PIPELINE_0,
  PipelineId_PIPELINE_1,
  PipelineId'
  #-}
