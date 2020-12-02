{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ExecutionTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ExecutionTrigger where

import Network.AWS.CodePipeline.Types.TriggerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The interaction or event that started a pipeline execution.
--
--
--
-- /See:/ 'executionTrigger' smart constructor.
data ExecutionTrigger = ExecutionTrigger'
  { _etTriggerType ::
      !(Maybe TriggerType),
    _etTriggerDetail :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etTriggerType' - The type of change-detection method, command, or user interaction that started a pipeline execution.
--
-- * 'etTriggerDetail' - Detail related to the event that started a pipeline execution, such as the webhook ARN of the webhook that triggered the pipeline execution or the user ARN for a user-initiated @start-pipeline-execution@ CLI command.
executionTrigger ::
  ExecutionTrigger
executionTrigger =
  ExecutionTrigger'
    { _etTriggerType = Nothing,
      _etTriggerDetail = Nothing
    }

-- | The type of change-detection method, command, or user interaction that started a pipeline execution.
etTriggerType :: Lens' ExecutionTrigger (Maybe TriggerType)
etTriggerType = lens _etTriggerType (\s a -> s {_etTriggerType = a})

-- | Detail related to the event that started a pipeline execution, such as the webhook ARN of the webhook that triggered the pipeline execution or the user ARN for a user-initiated @start-pipeline-execution@ CLI command.
etTriggerDetail :: Lens' ExecutionTrigger (Maybe Text)
etTriggerDetail = lens _etTriggerDetail (\s a -> s {_etTriggerDetail = a})

instance FromJSON ExecutionTrigger where
  parseJSON =
    withObject
      "ExecutionTrigger"
      ( \x ->
          ExecutionTrigger'
            <$> (x .:? "triggerType") <*> (x .:? "triggerDetail")
      )

instance Hashable ExecutionTrigger

instance NFData ExecutionTrigger
