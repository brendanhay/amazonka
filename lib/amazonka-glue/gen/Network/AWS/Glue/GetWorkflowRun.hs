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
-- Module      : Network.AWS.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run.
module Network.AWS.Glue.GetWorkflowRun
  ( -- * Creating a Request
    getWorkflowRun,
    GetWorkflowRun,

    -- * Request Lenses
    gwrwIncludeGraph,
    gwrwName,
    gwrwRunId,

    -- * Destructuring the Response
    getWorkflowRunResponse,
    GetWorkflowRunResponse,

    -- * Response Lenses
    gwrwrsRun,
    gwrwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'
  { _gwrwIncludeGraph ::
      !(Maybe Bool),
    _gwrwName :: !Text,
    _gwrwRunId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflowRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwrwIncludeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- * 'gwrwName' - Name of the workflow being run.
--
-- * 'gwrwRunId' - The ID of the workflow run.
getWorkflowRun ::
  -- | 'gwrwName'
  Text ->
  -- | 'gwrwRunId'
  Text ->
  GetWorkflowRun
getWorkflowRun pName_ pRunId_ =
  GetWorkflowRun'
    { _gwrwIncludeGraph = Nothing,
      _gwrwName = pName_,
      _gwrwRunId = pRunId_
    }

-- | Specifies whether to include the workflow graph in response or not.
gwrwIncludeGraph :: Lens' GetWorkflowRun (Maybe Bool)
gwrwIncludeGraph = lens _gwrwIncludeGraph (\s a -> s {_gwrwIncludeGraph = a})

-- | Name of the workflow being run.
gwrwName :: Lens' GetWorkflowRun Text
gwrwName = lens _gwrwName (\s a -> s {_gwrwName = a})

-- | The ID of the workflow run.
gwrwRunId :: Lens' GetWorkflowRun Text
gwrwRunId = lens _gwrwRunId (\s a -> s {_gwrwRunId = a})

instance AWSRequest GetWorkflowRun where
  type Rs GetWorkflowRun = GetWorkflowRunResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetWorkflowRunResponse' <$> (x .?> "Run") <*> (pure (fromEnum s))
      )

instance Hashable GetWorkflowRun

instance NFData GetWorkflowRun

instance ToHeaders GetWorkflowRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetWorkflowRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetWorkflowRun where
  toJSON GetWorkflowRun' {..} =
    object
      ( catMaybes
          [ ("IncludeGraph" .=) <$> _gwrwIncludeGraph,
            Just ("Name" .= _gwrwName),
            Just ("RunId" .= _gwrwRunId)
          ]
      )

instance ToPath GetWorkflowRun where
  toPath = const "/"

instance ToQuery GetWorkflowRun where
  toQuery = const mempty

-- | /See:/ 'getWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'
  { _gwrwrsRun ::
      !(Maybe WorkflowRun),
    _gwrwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetWorkflowRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gwrwrsRun' - The requested workflow run metadata.
--
-- * 'gwrwrsResponseStatus' - -- | The response status code.
getWorkflowRunResponse ::
  -- | 'gwrwrsResponseStatus'
  Int ->
  GetWorkflowRunResponse
getWorkflowRunResponse pResponseStatus_ =
  GetWorkflowRunResponse'
    { _gwrwrsRun = Nothing,
      _gwrwrsResponseStatus = pResponseStatus_
    }

-- | The requested workflow run metadata.
gwrwrsRun :: Lens' GetWorkflowRunResponse (Maybe WorkflowRun)
gwrwrsRun = lens _gwrwrsRun (\s a -> s {_gwrwrsRun = a})

-- | -- | The response status code.
gwrwrsResponseStatus :: Lens' GetWorkflowRunResponse Int
gwrwrsResponseStatus = lens _gwrwrsResponseStatus (\s a -> s {_gwrwrsResponseStatus = a})

instance NFData GetWorkflowRunResponse
