{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RenderableTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RenderableTask where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains input values for a task.
--
--
--
-- /See:/ 'renderableTask' smart constructor.
newtype RenderableTask = RenderableTask' {_rtInput :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RenderableTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtInput' - A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
renderableTask ::
  -- | 'rtInput'
  Text ->
  RenderableTask
renderableTask pInput_ = RenderableTask' {_rtInput = pInput_}

-- | A JSON object that contains values for the variables defined in the template. It is made available to the template under the substitution variable @task.input@ . For example, if you define a variable @task.input.text@ in your template, you can supply the variable in the JSON object as @"text": "sample text"@ .
rtInput :: Lens' RenderableTask Text
rtInput = lens _rtInput (\s a -> s {_rtInput = a})

instance Hashable RenderableTask

instance NFData RenderableTask

instance ToJSON RenderableTask where
  toJSON RenderableTask' {..} =
    object (catMaybes [Just ("Input" .= _rtInput)])
