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
-- Module      : Network.AWS.EMR.StartNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a notebook execution.
module Network.AWS.EMR.StartNotebookExecution
  ( -- * Creating a Request
    startNotebookExecution,
    StartNotebookExecution,

    -- * Request Lenses
    sneNotebookInstanceSecurityGroupId,
    sneNotebookExecutionName,
    sneNotebookParams,
    sneTags,
    sneEditorId,
    sneRelativePath,
    sneExecutionEngine,
    sneServiceRole,

    -- * Destructuring the Response
    startNotebookExecutionResponse,
    StartNotebookExecutionResponse,

    -- * Response Lenses
    snersNotebookExecutionId,
    snersResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startNotebookExecution' smart constructor.
data StartNotebookExecution = StartNotebookExecution'
  { _sneNotebookInstanceSecurityGroupId ::
      !(Maybe Text),
    _sneNotebookExecutionName :: !(Maybe Text),
    _sneNotebookParams :: !(Maybe Text),
    _sneTags :: !(Maybe [Tag]),
    _sneEditorId :: !Text,
    _sneRelativePath :: !Text,
    _sneExecutionEngine :: !ExecutionEngineConfig,
    _sneServiceRole :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartNotebookExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sneNotebookInstanceSecurityGroupId' - The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
--
-- * 'sneNotebookExecutionName' - An optional name for the notebook execution.
--
-- * 'sneNotebookParams' - Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
--
-- * 'sneTags' - A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
--
-- * 'sneEditorId' - The unique identifier of the EMR Notebook to use for notebook execution.
--
-- * 'sneRelativePath' - The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
--
-- * 'sneExecutionEngine' - Specifies the execution engine (cluster) that runs the notebook execution.
--
-- * 'sneServiceRole' - The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
startNotebookExecution ::
  -- | 'sneEditorId'
  Text ->
  -- | 'sneRelativePath'
  Text ->
  -- | 'sneExecutionEngine'
  ExecutionEngineConfig ->
  -- | 'sneServiceRole'
  Text ->
  StartNotebookExecution
startNotebookExecution
  pEditorId_
  pRelativePath_
  pExecutionEngine_
  pServiceRole_ =
    StartNotebookExecution'
      { _sneNotebookInstanceSecurityGroupId =
          Nothing,
        _sneNotebookExecutionName = Nothing,
        _sneNotebookParams = Nothing,
        _sneTags = Nothing,
        _sneEditorId = pEditorId_,
        _sneRelativePath = pRelativePath_,
        _sneExecutionEngine = pExecutionEngine_,
        _sneServiceRole = pServiceRole_
      }

-- | The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
sneNotebookInstanceSecurityGroupId :: Lens' StartNotebookExecution (Maybe Text)
sneNotebookInstanceSecurityGroupId = lens _sneNotebookInstanceSecurityGroupId (\s a -> s {_sneNotebookInstanceSecurityGroupId = a})

-- | An optional name for the notebook execution.
sneNotebookExecutionName :: Lens' StartNotebookExecution (Maybe Text)
sneNotebookExecutionName = lens _sneNotebookExecutionName (\s a -> s {_sneNotebookExecutionName = a})

-- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
sneNotebookParams :: Lens' StartNotebookExecution (Maybe Text)
sneNotebookParams = lens _sneNotebookParams (\s a -> s {_sneNotebookParams = a})

-- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
sneTags :: Lens' StartNotebookExecution [Tag]
sneTags = lens _sneTags (\s a -> s {_sneTags = a}) . _Default . _Coerce

-- | The unique identifier of the EMR Notebook to use for notebook execution.
sneEditorId :: Lens' StartNotebookExecution Text
sneEditorId = lens _sneEditorId (\s a -> s {_sneEditorId = a})

-- | The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
sneRelativePath :: Lens' StartNotebookExecution Text
sneRelativePath = lens _sneRelativePath (\s a -> s {_sneRelativePath = a})

-- | Specifies the execution engine (cluster) that runs the notebook execution.
sneExecutionEngine :: Lens' StartNotebookExecution ExecutionEngineConfig
sneExecutionEngine = lens _sneExecutionEngine (\s a -> s {_sneExecutionEngine = a})

-- | The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
sneServiceRole :: Lens' StartNotebookExecution Text
sneServiceRole = lens _sneServiceRole (\s a -> s {_sneServiceRole = a})

instance AWSRequest StartNotebookExecution where
  type Rs StartNotebookExecution = StartNotebookExecutionResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          StartNotebookExecutionResponse'
            <$> (x .?> "NotebookExecutionId") <*> (pure (fromEnum s))
      )

instance Hashable StartNotebookExecution

instance NFData StartNotebookExecution

instance ToHeaders StartNotebookExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.StartNotebookExecution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartNotebookExecution where
  toJSON StartNotebookExecution' {..} =
    object
      ( catMaybes
          [ ("NotebookInstanceSecurityGroupId" .=)
              <$> _sneNotebookInstanceSecurityGroupId,
            ("NotebookExecutionName" .=) <$> _sneNotebookExecutionName,
            ("NotebookParams" .=) <$> _sneNotebookParams,
            ("Tags" .=) <$> _sneTags,
            Just ("EditorId" .= _sneEditorId),
            Just ("RelativePath" .= _sneRelativePath),
            Just ("ExecutionEngine" .= _sneExecutionEngine),
            Just ("ServiceRole" .= _sneServiceRole)
          ]
      )

instance ToPath StartNotebookExecution where
  toPath = const "/"

instance ToQuery StartNotebookExecution where
  toQuery = const mempty

-- | /See:/ 'startNotebookExecutionResponse' smart constructor.
data StartNotebookExecutionResponse = StartNotebookExecutionResponse'
  { _snersNotebookExecutionId ::
      !(Maybe Text),
    _snersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartNotebookExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snersNotebookExecutionId' - The unique identifier of the notebook execution.
--
-- * 'snersResponseStatus' - -- | The response status code.
startNotebookExecutionResponse ::
  -- | 'snersResponseStatus'
  Int ->
  StartNotebookExecutionResponse
startNotebookExecutionResponse pResponseStatus_ =
  StartNotebookExecutionResponse'
    { _snersNotebookExecutionId =
        Nothing,
      _snersResponseStatus = pResponseStatus_
    }

-- | The unique identifier of the notebook execution.
snersNotebookExecutionId :: Lens' StartNotebookExecutionResponse (Maybe Text)
snersNotebookExecutionId = lens _snersNotebookExecutionId (\s a -> s {_snersNotebookExecutionId = a})

-- | -- | The response status code.
snersResponseStatus :: Lens' StartNotebookExecutionResponse Int
snersResponseStatus = lens _snersResponseStatus (\s a -> s {_snersResponseStatus = a})

instance NFData StartNotebookExecutionResponse
