{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filter values for the action execution.
--
--
--
-- /See:/ 'actionExecutionFilter' smart constructor.
newtype ActionExecutionFilter = ActionExecutionFilter'
  { _aefPipelineExecutionId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionExecutionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aefPipelineExecutionId' - The pipeline execution ID used to filter action execution history.
actionExecutionFilter ::
  ActionExecutionFilter
actionExecutionFilter =
  ActionExecutionFilter' {_aefPipelineExecutionId = Nothing}

-- | The pipeline execution ID used to filter action execution history.
aefPipelineExecutionId :: Lens' ActionExecutionFilter (Maybe Text)
aefPipelineExecutionId = lens _aefPipelineExecutionId (\s a -> s {_aefPipelineExecutionId = a})

instance Hashable ActionExecutionFilter

instance NFData ActionExecutionFilter

instance ToJSON ActionExecutionFilter where
  toJSON ActionExecutionFilter' {..} =
    object
      ( catMaybes
          [("pipelineExecutionId" .=) <$> _aefPipelineExecutionId]
      )
