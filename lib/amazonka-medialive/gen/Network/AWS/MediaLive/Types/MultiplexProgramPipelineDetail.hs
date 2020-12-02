{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The current source for one of the pipelines in the multiplex.
--
-- /See:/ 'multiplexProgramPipelineDetail' smart constructor.
data MultiplexProgramPipelineDetail = MultiplexProgramPipelineDetail'
  { _mppdPipelineId ::
      !(Maybe Text),
    _mppdActiveChannelPipeline ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgramPipelineDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mppdPipelineId' - Identifies a specific pipeline in the multiplex.
--
-- * 'mppdActiveChannelPipeline' - Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
multiplexProgramPipelineDetail ::
  MultiplexProgramPipelineDetail
multiplexProgramPipelineDetail =
  MultiplexProgramPipelineDetail'
    { _mppdPipelineId = Nothing,
      _mppdActiveChannelPipeline = Nothing
    }

-- | Identifies a specific pipeline in the multiplex.
mppdPipelineId :: Lens' MultiplexProgramPipelineDetail (Maybe Text)
mppdPipelineId = lens _mppdPipelineId (\s a -> s {_mppdPipelineId = a})

-- | Identifies the channel pipeline that is currently active for the pipeline (identified by PipelineId) in the multiplex.
mppdActiveChannelPipeline :: Lens' MultiplexProgramPipelineDetail (Maybe Text)
mppdActiveChannelPipeline = lens _mppdActiveChannelPipeline (\s a -> s {_mppdActiveChannelPipeline = a})

instance FromJSON MultiplexProgramPipelineDetail where
  parseJSON =
    withObject
      "MultiplexProgramPipelineDetail"
      ( \x ->
          MultiplexProgramPipelineDetail'
            <$> (x .:? "pipelineId") <*> (x .:? "activeChannelPipeline")
      )

instance Hashable MultiplexProgramPipelineDetail

instance NFData MultiplexProgramPipelineDetail
