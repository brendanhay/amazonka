{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PipelinePauseStateSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelinePauseStateSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.PipelineId
import Network.AWS.Prelude

-- | Settings for pausing a pipeline.
--
-- /See:/ 'pipelinePauseStateSettings' smart constructor.
newtype PipelinePauseStateSettings = PipelinePauseStateSettings'
  { _ppssPipelineId ::
      PipelineId
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelinePauseStateSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppssPipelineId' - Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
pipelinePauseStateSettings ::
  -- | 'ppssPipelineId'
  PipelineId ->
  PipelinePauseStateSettings
pipelinePauseStateSettings pPipelineId_ =
  PipelinePauseStateSettings' {_ppssPipelineId = pPipelineId_}

-- | Pipeline ID to pause ("PIPELINE_0" or "PIPELINE_1").
ppssPipelineId :: Lens' PipelinePauseStateSettings PipelineId
ppssPipelineId = lens _ppssPipelineId (\s a -> s {_ppssPipelineId = a})

instance FromJSON PipelinePauseStateSettings where
  parseJSON =
    withObject
      "PipelinePauseStateSettings"
      (\x -> PipelinePauseStateSettings' <$> (x .: "pipelineId"))

instance Hashable PipelinePauseStateSettings

instance NFData PipelinePauseStateSettings

instance ToJSON PipelinePauseStateSettings where
  toJSON PipelinePauseStateSettings' {..} =
    object (catMaybes [Just ("pipelineId" .= _ppssPipelineId)])
