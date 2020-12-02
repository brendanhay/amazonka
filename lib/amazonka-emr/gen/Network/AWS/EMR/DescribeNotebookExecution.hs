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
-- Module      : Network.AWS.EMR.DescribeNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details of a notebook execution.
module Network.AWS.EMR.DescribeNotebookExecution
  ( -- * Creating a Request
    describeNotebookExecution,
    DescribeNotebookExecution,

    -- * Request Lenses
    dneNotebookExecutionId,

    -- * Destructuring the Response
    describeNotebookExecutionResponse,
    DescribeNotebookExecutionResponse,

    -- * Response Lenses
    dnersNotebookExecution,
    dnersResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeNotebookExecution' smart constructor.
newtype DescribeNotebookExecution = DescribeNotebookExecution'
  { _dneNotebookExecutionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeNotebookExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dneNotebookExecutionId' - The unique identifier of the notebook execution.
describeNotebookExecution ::
  -- | 'dneNotebookExecutionId'
  Text ->
  DescribeNotebookExecution
describeNotebookExecution pNotebookExecutionId_ =
  DescribeNotebookExecution'
    { _dneNotebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
dneNotebookExecutionId :: Lens' DescribeNotebookExecution Text
dneNotebookExecutionId = lens _dneNotebookExecutionId (\s a -> s {_dneNotebookExecutionId = a})

instance AWSRequest DescribeNotebookExecution where
  type
    Rs DescribeNotebookExecution =
      DescribeNotebookExecutionResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          DescribeNotebookExecutionResponse'
            <$> (x .?> "NotebookExecution") <*> (pure (fromEnum s))
      )

instance Hashable DescribeNotebookExecution

instance NFData DescribeNotebookExecution

instance ToHeaders DescribeNotebookExecution where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.DescribeNotebookExecution" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeNotebookExecution where
  toJSON DescribeNotebookExecution' {..} =
    object
      ( catMaybes
          [Just ("NotebookExecutionId" .= _dneNotebookExecutionId)]
      )

instance ToPath DescribeNotebookExecution where
  toPath = const "/"

instance ToQuery DescribeNotebookExecution where
  toQuery = const mempty

-- | /See:/ 'describeNotebookExecutionResponse' smart constructor.
data DescribeNotebookExecutionResponse = DescribeNotebookExecutionResponse'
  { _dnersNotebookExecution ::
      !( Maybe
           NotebookExecution
       ),
    _dnersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeNotebookExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnersNotebookExecution' - Properties of the notebook execution.
--
-- * 'dnersResponseStatus' - -- | The response status code.
describeNotebookExecutionResponse ::
  -- | 'dnersResponseStatus'
  Int ->
  DescribeNotebookExecutionResponse
describeNotebookExecutionResponse pResponseStatus_ =
  DescribeNotebookExecutionResponse'
    { _dnersNotebookExecution =
        Nothing,
      _dnersResponseStatus = pResponseStatus_
    }

-- | Properties of the notebook execution.
dnersNotebookExecution :: Lens' DescribeNotebookExecutionResponse (Maybe NotebookExecution)
dnersNotebookExecution = lens _dnersNotebookExecution (\s a -> s {_dnersNotebookExecution = a})

-- | -- | The response status code.
dnersResponseStatus :: Lens' DescribeNotebookExecutionResponse Int
dnersResponseStatus = lens _dnersResponseStatus (\s a -> s {_dnersResponseStatus = a})

instance NFData DescribeNotebookExecutionResponse
