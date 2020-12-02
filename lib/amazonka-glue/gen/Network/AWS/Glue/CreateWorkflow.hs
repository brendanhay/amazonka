{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new workflow.
module Network.AWS.Glue.CreateWorkflow
  ( -- * Creating a Request
    createWorkflow,
    CreateWorkflow,

    -- * Request Lenses
    cwMaxConcurrentRuns,
    cwDefaultRunProperties,
    cwDescription,
    cwTags,
    cwName,

    -- * Destructuring the Response
    createWorkflowResponse,
    CreateWorkflowResponse,

    -- * Response Lenses
    cwrsName,
    cwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { _cwMaxConcurrentRuns ::
      !(Maybe Int),
    _cwDefaultRunProperties :: !(Maybe (Map Text (Text))),
    _cwDescription :: !(Maybe Text),
    _cwTags :: !(Maybe (Map Text (Text))),
    _cwName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWorkflow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwMaxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- * 'cwDefaultRunProperties' - A collection of properties to be used as part of each execution of the workflow.
--
-- * 'cwDescription' - A description of the workflow.
--
-- * 'cwTags' - The tags to be used with this workflow.
--
-- * 'cwName' - The name to be assigned to the workflow. It should be unique within your account.
createWorkflow ::
  -- | 'cwName'
  Text ->
  CreateWorkflow
createWorkflow pName_ =
  CreateWorkflow'
    { _cwMaxConcurrentRuns = Nothing,
      _cwDefaultRunProperties = Nothing,
      _cwDescription = Nothing,
      _cwTags = Nothing,
      _cwName = pName_
    }

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
cwMaxConcurrentRuns :: Lens' CreateWorkflow (Maybe Int)
cwMaxConcurrentRuns = lens _cwMaxConcurrentRuns (\s a -> s {_cwMaxConcurrentRuns = a})

-- | A collection of properties to be used as part of each execution of the workflow.
cwDefaultRunProperties :: Lens' CreateWorkflow (HashMap Text (Text))
cwDefaultRunProperties = lens _cwDefaultRunProperties (\s a -> s {_cwDefaultRunProperties = a}) . _Default . _Map

-- | A description of the workflow.
cwDescription :: Lens' CreateWorkflow (Maybe Text)
cwDescription = lens _cwDescription (\s a -> s {_cwDescription = a})

-- | The tags to be used with this workflow.
cwTags :: Lens' CreateWorkflow (HashMap Text (Text))
cwTags = lens _cwTags (\s a -> s {_cwTags = a}) . _Default . _Map

-- | The name to be assigned to the workflow. It should be unique within your account.
cwName :: Lens' CreateWorkflow Text
cwName = lens _cwName (\s a -> s {_cwName = a})

instance AWSRequest CreateWorkflow where
  type Rs CreateWorkflow = CreateWorkflowResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          CreateWorkflowResponse' <$> (x .?> "Name") <*> (pure (fromEnum s))
      )

instance Hashable CreateWorkflow

instance NFData CreateWorkflow

instance ToHeaders CreateWorkflow where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.CreateWorkflow" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateWorkflow where
  toJSON CreateWorkflow' {..} =
    object
      ( catMaybes
          [ ("MaxConcurrentRuns" .=) <$> _cwMaxConcurrentRuns,
            ("DefaultRunProperties" .=) <$> _cwDefaultRunProperties,
            ("Description" .=) <$> _cwDescription,
            ("Tags" .=) <$> _cwTags,
            Just ("Name" .= _cwName)
          ]
      )

instance ToPath CreateWorkflow where
  toPath = const "/"

instance ToQuery CreateWorkflow where
  toQuery = const mempty

-- | /See:/ 'createWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { _cwrsName ::
      !(Maybe Text),
    _cwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWorkflowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwrsName' - The name of the workflow which was provided as part of the request.
--
-- * 'cwrsResponseStatus' - -- | The response status code.
createWorkflowResponse ::
  -- | 'cwrsResponseStatus'
  Int ->
  CreateWorkflowResponse
createWorkflowResponse pResponseStatus_ =
  CreateWorkflowResponse'
    { _cwrsName = Nothing,
      _cwrsResponseStatus = pResponseStatus_
    }

-- | The name of the workflow which was provided as part of the request.
cwrsName :: Lens' CreateWorkflowResponse (Maybe Text)
cwrsName = lens _cwrsName (\s a -> s {_cwrsName = a})

-- | -- | The response status code.
cwrsResponseStatus :: Lens' CreateWorkflowResponse Int
cwrsResponseStatus = lens _cwrsResponseStatus (\s a -> s {_cwrsResponseStatus = a})

instance NFData CreateWorkflowResponse
