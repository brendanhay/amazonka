{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StopExecutionTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StopExecutionTrigger where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The interaction that stopped a pipeline execution.
--
--
--
-- /See:/ 'stopExecutionTrigger' smart constructor.
newtype StopExecutionTrigger = StopExecutionTrigger'
  { _setReason ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopExecutionTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'setReason' - The user-specified reason the pipeline was stopped.
stopExecutionTrigger ::
  StopExecutionTrigger
stopExecutionTrigger = StopExecutionTrigger' {_setReason = Nothing}

-- | The user-specified reason the pipeline was stopped.
setReason :: Lens' StopExecutionTrigger (Maybe Text)
setReason = lens _setReason (\s a -> s {_setReason = a})

instance FromJSON StopExecutionTrigger where
  parseJSON =
    withObject
      "StopExecutionTrigger"
      (\x -> StopExecutionTrigger' <$> (x .:? "reason"))

instance Hashable StopExecutionTrigger

instance NFData StopExecutionTrigger
