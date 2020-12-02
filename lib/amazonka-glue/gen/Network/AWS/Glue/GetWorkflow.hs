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
-- Module      : Network.AWS.Glue.GetWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves resource metadata for a workflow.
module Network.AWS.Glue.GetWorkflow
  ( -- * Creating a Request
    getWorkflow,
    GetWorkflow,

    -- * Request Lenses
    gwIncludeGraph,
    gwName,

    -- * Destructuring the Response
    getWorkflowResponse,
    GetWorkflowResponse,

    -- * Response Lenses
    gwrsWorkflow,
    gwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { _gwIncludeGraph :: !(Maybe Bool),
    _gwName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwIncludeGraph' - Specifies whether to include a graph when returning the workflow resource metadata.
--
-- * 'gwName' - The name of the workflow to retrieve.
getWorkflow ::
  -- | 'gwName'
  Text ->
  GetWorkflow
getWorkflow pName_ =
  GetWorkflow' {_gwIncludeGraph = Nothing, _gwName = pName_}

-- | Specifies whether to include a graph when returning the workflow resource metadata.
gwIncludeGraph :: Lens' GetWorkflow (Maybe Bool)
gwIncludeGraph = lens _gwIncludeGraph (\s a -> s {_gwIncludeGraph = a})

-- | The name of the workflow to retrieve.
gwName :: Lens' GetWorkflow Text
gwName = lens _gwName (\s a -> s {_gwName = a})

instance AWSRequest GetWorkflow where
  type Rs GetWorkflow = GetWorkflowResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            <$> (x .?> "Workflow") <*> (pure (fromEnum s))
      )

instance Hashable GetWorkflow

instance NFData GetWorkflow

instance ToHeaders GetWorkflow where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetWorkflow" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetWorkflow where
  toJSON GetWorkflow' {..} =
    object
      ( catMaybes
          [ ("IncludeGraph" .=) <$> _gwIncludeGraph,
            Just ("Name" .= _gwName)
          ]
      )

instance ToPath GetWorkflow where
  toPath = const "/"

instance ToQuery GetWorkflow where
  toQuery = const mempty

-- | /See:/ 'getWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { _gwrsWorkflow ::
      !(Maybe Workflow),
    _gwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwrsWorkflow' - The resource metadata for the workflow.
--
-- * 'gwrsResponseStatus' - -- | The response status code.
getWorkflowResponse ::
  -- | 'gwrsResponseStatus'
  Int ->
  GetWorkflowResponse
getWorkflowResponse pResponseStatus_ =
  GetWorkflowResponse'
    { _gwrsWorkflow = Nothing,
      _gwrsResponseStatus = pResponseStatus_
    }

-- | The resource metadata for the workflow.
gwrsWorkflow :: Lens' GetWorkflowResponse (Maybe Workflow)
gwrsWorkflow = lens _gwrsWorkflow (\s a -> s {_gwrsWorkflow = a})

-- | -- | The response status code.
gwrsResponseStatus :: Lens' GetWorkflowResponse Int
gwrsResponseStatus = lens _gwrsResponseStatus (\s a -> s {_gwrsResponseStatus = a})

instance NFData GetWorkflowResponse
