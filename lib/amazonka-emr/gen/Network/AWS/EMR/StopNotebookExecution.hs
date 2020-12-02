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
-- Module      : Network.AWS.EMR.StopNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a notebook execution.
module Network.AWS.EMR.StopNotebookExecution
  ( -- * Creating a Request
    stopNotebookExecution,
    StopNotebookExecution,

    -- * Request Lenses
    sneNotebookExecutionId,

    -- * Destructuring the Response
    stopNotebookExecutionResponse,
    StopNotebookExecutionResponse,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopNotebookExecution' smart constructor.
newtype StopNotebookExecution = StopNotebookExecution'
  { _sneNotebookExecutionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopNotebookExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sneNotebookExecutionId' - The unique identifier of the notebook execution.
stopNotebookExecution ::
  -- | 'sneNotebookExecutionId'
  Text ->
  StopNotebookExecution
stopNotebookExecution pNotebookExecutionId_ =
  StopNotebookExecution'
    { _sneNotebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
sneNotebookExecutionId :: Lens' StopNotebookExecution Text
sneNotebookExecutionId = lens _sneNotebookExecutionId (\s a -> s {_sneNotebookExecutionId = a})

instance AWSRequest StopNotebookExecution where
  type Rs StopNotebookExecution = StopNotebookExecutionResponse
  request = postJSON emr
  response = receiveNull StopNotebookExecutionResponse'

instance Hashable StopNotebookExecution

instance NFData StopNotebookExecution

instance ToHeaders StopNotebookExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.StopNotebookExecution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StopNotebookExecution where
  toJSON StopNotebookExecution' {..} =
    object
      ( catMaybes
          [Just ("NotebookExecutionId" .= _sneNotebookExecutionId)]
      )

instance ToPath StopNotebookExecution where
  toPath = const "/"

instance ToQuery StopNotebookExecution where
  toQuery = const mempty

-- | /See:/ 'stopNotebookExecutionResponse' smart constructor.
data StopNotebookExecutionResponse = StopNotebookExecutionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopNotebookExecutionResponse' with the minimum fields required to make a request.
stopNotebookExecutionResponse ::
  StopNotebookExecutionResponse
stopNotebookExecutionResponse = StopNotebookExecutionResponse'

instance NFData StopNotebookExecutionResponse
