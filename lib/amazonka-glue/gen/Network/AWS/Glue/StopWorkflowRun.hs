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
-- Module      : Network.AWS.Glue.StopWorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of the specified workflow run.
module Network.AWS.Glue.StopWorkflowRun
  ( -- * Creating a Request
    stopWorkflowRun,
    StopWorkflowRun,

    -- * Request Lenses
    swrwName,
    swrwRunId,

    -- * Destructuring the Response
    stopWorkflowRunResponse,
    StopWorkflowRunResponse,

    -- * Response Lenses
    swrwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { _swrwName :: !Text,
    _swrwRunId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopWorkflowRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swrwName' - The name of the workflow to stop.
--
-- * 'swrwRunId' - The ID of the workflow run to stop.
stopWorkflowRun ::
  -- | 'swrwName'
  Text ->
  -- | 'swrwRunId'
  Text ->
  StopWorkflowRun
stopWorkflowRun pName_ pRunId_ =
  StopWorkflowRun' {_swrwName = pName_, _swrwRunId = pRunId_}

-- | The name of the workflow to stop.
swrwName :: Lens' StopWorkflowRun Text
swrwName = lens _swrwName (\s a -> s {_swrwName = a})

-- | The ID of the workflow run to stop.
swrwRunId :: Lens' StopWorkflowRun Text
swrwRunId = lens _swrwRunId (\s a -> s {_swrwRunId = a})

instance AWSRequest StopWorkflowRun where
  type Rs StopWorkflowRun = StopWorkflowRunResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> StopWorkflowRunResponse' <$> (pure (fromEnum s)))

instance Hashable StopWorkflowRun

instance NFData StopWorkflowRun

instance ToHeaders StopWorkflowRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.StopWorkflowRun" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopWorkflowRun where
  toJSON StopWorkflowRun' {..} =
    object
      ( catMaybes
          [Just ("Name" .= _swrwName), Just ("RunId" .= _swrwRunId)]
      )

instance ToPath StopWorkflowRun where
  toPath = const "/"

instance ToQuery StopWorkflowRun where
  toQuery = const mempty

-- | /See:/ 'stopWorkflowRunResponse' smart constructor.
newtype StopWorkflowRunResponse = StopWorkflowRunResponse'
  { _swrwrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopWorkflowRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swrwrsResponseStatus' - -- | The response status code.
stopWorkflowRunResponse ::
  -- | 'swrwrsResponseStatus'
  Int ->
  StopWorkflowRunResponse
stopWorkflowRunResponse pResponseStatus_ =
  StopWorkflowRunResponse'
    { _swrwrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
swrwrsResponseStatus :: Lens' StopWorkflowRunResponse Int
swrwrsResponseStatus = lens _swrwrsResponseStatus (\s a -> s {_swrwrsResponseStatus = a})

instance NFData StopWorkflowRunResponse
