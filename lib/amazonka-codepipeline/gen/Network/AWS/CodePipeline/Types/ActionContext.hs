{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the context of an action in the stage of a pipeline to a job worker.
--
--
--
-- /See:/ 'actionContext' smart constructor.
data ActionContext = ActionContext'
  { _acName :: !(Maybe Text),
    _acActionExecutionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acName' - The name of the action in the context of a job.
--
-- * 'acActionExecutionId' - The system-generated unique ID that corresponds to an action's execution.
actionContext ::
  ActionContext
actionContext =
  ActionContext' {_acName = Nothing, _acActionExecutionId = Nothing}

-- | The name of the action in the context of a job.
acName :: Lens' ActionContext (Maybe Text)
acName = lens _acName (\s a -> s {_acName = a})

-- | The system-generated unique ID that corresponds to an action's execution.
acActionExecutionId :: Lens' ActionContext (Maybe Text)
acActionExecutionId = lens _acActionExecutionId (\s a -> s {_acActionExecutionId = a})

instance FromJSON ActionContext where
  parseJSON =
    withObject
      "ActionContext"
      ( \x ->
          ActionContext' <$> (x .:? "name") <*> (x .:? "actionExecutionId")
      )

instance Hashable ActionContext

instance NFData ActionContext
