{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.PipelineIdName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.PipelineIdName where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the name and identifier of a pipeline.
--
--
--
-- /See:/ 'pipelineIdName' smart constructor.
data PipelineIdName = PipelineIdName'
  { _pinName :: !(Maybe Text),
    _pinId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PipelineIdName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pinName' - The name of the pipeline.
--
-- * 'pinId' - The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
pipelineIdName ::
  PipelineIdName
pipelineIdName =
  PipelineIdName' {_pinName = Nothing, _pinId = Nothing}

-- | The name of the pipeline.
pinName :: Lens' PipelineIdName (Maybe Text)
pinName = lens _pinName (\s a -> s {_pinName = a})

-- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
pinId :: Lens' PipelineIdName (Maybe Text)
pinId = lens _pinId (\s a -> s {_pinId = a})

instance FromJSON PipelineIdName where
  parseJSON =
    withObject
      "PipelineIdName"
      (\x -> PipelineIdName' <$> (x .:? "name") <*> (x .:? "id"))

instance Hashable PipelineIdName

instance NFData PipelineIdName
