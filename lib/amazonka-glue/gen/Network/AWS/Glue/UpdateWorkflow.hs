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
-- Module      : Network.AWS.Glue.UpdateWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing workflow.
module Network.AWS.Glue.UpdateWorkflow
  ( -- * Creating a Request
    updateWorkflow,
    UpdateWorkflow,

    -- * Request Lenses
    uwMaxConcurrentRuns,
    uwDefaultRunProperties,
    uwDescription,
    uwName,

    -- * Destructuring the Response
    updateWorkflowResponse,
    UpdateWorkflowResponse,

    -- * Response Lenses
    uwrsName,
    uwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { _uwMaxConcurrentRuns ::
      !(Maybe Int),
    _uwDefaultRunProperties :: !(Maybe (Map Text (Text))),
    _uwDescription :: !(Maybe Text),
    _uwName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkflow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwMaxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- * 'uwDefaultRunProperties' - A collection of properties to be used as part of each execution of the workflow.
--
-- * 'uwDescription' - The description of the workflow.
--
-- * 'uwName' - Name of the workflow to be updated.
updateWorkflow ::
  -- | 'uwName'
  Text ->
  UpdateWorkflow
updateWorkflow pName_ =
  UpdateWorkflow'
    { _uwMaxConcurrentRuns = Nothing,
      _uwDefaultRunProperties = Nothing,
      _uwDescription = Nothing,
      _uwName = pName_
    }

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
uwMaxConcurrentRuns :: Lens' UpdateWorkflow (Maybe Int)
uwMaxConcurrentRuns = lens _uwMaxConcurrentRuns (\s a -> s {_uwMaxConcurrentRuns = a})

-- | A collection of properties to be used as part of each execution of the workflow.
uwDefaultRunProperties :: Lens' UpdateWorkflow (HashMap Text (Text))
uwDefaultRunProperties = lens _uwDefaultRunProperties (\s a -> s {_uwDefaultRunProperties = a}) . _Default . _Map

-- | The description of the workflow.
uwDescription :: Lens' UpdateWorkflow (Maybe Text)
uwDescription = lens _uwDescription (\s a -> s {_uwDescription = a})

-- | Name of the workflow to be updated.
uwName :: Lens' UpdateWorkflow Text
uwName = lens _uwName (\s a -> s {_uwName = a})

instance AWSRequest UpdateWorkflow where
  type Rs UpdateWorkflow = UpdateWorkflowResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          UpdateWorkflowResponse' <$> (x .?> "Name") <*> (pure (fromEnum s))
      )

instance Hashable UpdateWorkflow

instance NFData UpdateWorkflow

instance ToHeaders UpdateWorkflow where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.UpdateWorkflow" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateWorkflow where
  toJSON UpdateWorkflow' {..} =
    object
      ( catMaybes
          [ ("MaxConcurrentRuns" .=) <$> _uwMaxConcurrentRuns,
            ("DefaultRunProperties" .=) <$> _uwDefaultRunProperties,
            ("Description" .=) <$> _uwDescription,
            Just ("Name" .= _uwName)
          ]
      )

instance ToPath UpdateWorkflow where
  toPath = const "/"

instance ToQuery UpdateWorkflow where
  toQuery = const mempty

-- | /See:/ 'updateWorkflowResponse' smart constructor.
data UpdateWorkflowResponse = UpdateWorkflowResponse'
  { _uwrsName ::
      !(Maybe Text),
    _uwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkflowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwrsName' - The name of the workflow which was specified in input.
--
-- * 'uwrsResponseStatus' - -- | The response status code.
updateWorkflowResponse ::
  -- | 'uwrsResponseStatus'
  Int ->
  UpdateWorkflowResponse
updateWorkflowResponse pResponseStatus_ =
  UpdateWorkflowResponse'
    { _uwrsName = Nothing,
      _uwrsResponseStatus = pResponseStatus_
    }

-- | The name of the workflow which was specified in input.
uwrsName :: Lens' UpdateWorkflowResponse (Maybe Text)
uwrsName = lens _uwrsName (\s a -> s {_uwrsName = a})

-- | -- | The response status code.
uwrsResponseStatus :: Lens' UpdateWorkflowResponse Int
uwrsResponseStatus = lens _uwrsResponseStatus (\s a -> s {_uwrsResponseStatus = a})

instance NFData UpdateWorkflowResponse
