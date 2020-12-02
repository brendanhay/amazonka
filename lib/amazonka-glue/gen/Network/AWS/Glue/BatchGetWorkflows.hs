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
-- Module      : Network.AWS.Glue.BatchGetWorkflows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of workflow names. After calling the @ListWorkflows@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetWorkflows
  ( -- * Creating a Request
    batchGetWorkflows,
    BatchGetWorkflows,

    -- * Request Lenses
    bgwIncludeGraph,
    bgwNames,

    -- * Destructuring the Response
    batchGetWorkflowsResponse,
    BatchGetWorkflowsResponse,

    -- * Response Lenses
    bgwrsMissingWorkflows,
    bgwrsWorkflows,
    bgwrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetWorkflows' smart constructor.
data BatchGetWorkflows = BatchGetWorkflows'
  { _bgwIncludeGraph ::
      !(Maybe Bool),
    _bgwNames :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetWorkflows' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgwIncludeGraph' - Specifies whether to include a graph when returning the workflow resource metadata.
--
-- * 'bgwNames' - A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
batchGetWorkflows ::
  -- | 'bgwNames'
  NonEmpty Text ->
  BatchGetWorkflows
batchGetWorkflows pNames_ =
  BatchGetWorkflows'
    { _bgwIncludeGraph = Nothing,
      _bgwNames = _List1 # pNames_
    }

-- | Specifies whether to include a graph when returning the workflow resource metadata.
bgwIncludeGraph :: Lens' BatchGetWorkflows (Maybe Bool)
bgwIncludeGraph = lens _bgwIncludeGraph (\s a -> s {_bgwIncludeGraph = a})

-- | A list of workflow names, which may be the names returned from the @ListWorkflows@ operation.
bgwNames :: Lens' BatchGetWorkflows (NonEmpty Text)
bgwNames = lens _bgwNames (\s a -> s {_bgwNames = a}) . _List1

instance AWSRequest BatchGetWorkflows where
  type Rs BatchGetWorkflows = BatchGetWorkflowsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          BatchGetWorkflowsResponse'
            <$> (x .?> "MissingWorkflows")
            <*> (x .?> "Workflows")
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetWorkflows

instance NFData BatchGetWorkflows

instance ToHeaders BatchGetWorkflows where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.BatchGetWorkflows" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetWorkflows where
  toJSON BatchGetWorkflows' {..} =
    object
      ( catMaybes
          [ ("IncludeGraph" .=) <$> _bgwIncludeGraph,
            Just ("Names" .= _bgwNames)
          ]
      )

instance ToPath BatchGetWorkflows where
  toPath = const "/"

instance ToQuery BatchGetWorkflows where
  toQuery = const mempty

-- | /See:/ 'batchGetWorkflowsResponse' smart constructor.
data BatchGetWorkflowsResponse = BatchGetWorkflowsResponse'
  { _bgwrsMissingWorkflows ::
      !(Maybe (List1 Text)),
    _bgwrsWorkflows ::
      !(Maybe (List1 Workflow)),
    _bgwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetWorkflowsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgwrsMissingWorkflows' - A list of names of workflows not found.
--
-- * 'bgwrsWorkflows' - A list of workflow resource metadata.
--
-- * 'bgwrsResponseStatus' - -- | The response status code.
batchGetWorkflowsResponse ::
  -- | 'bgwrsResponseStatus'
  Int ->
  BatchGetWorkflowsResponse
batchGetWorkflowsResponse pResponseStatus_ =
  BatchGetWorkflowsResponse'
    { _bgwrsMissingWorkflows = Nothing,
      _bgwrsWorkflows = Nothing,
      _bgwrsResponseStatus = pResponseStatus_
    }

-- | A list of names of workflows not found.
bgwrsMissingWorkflows :: Lens' BatchGetWorkflowsResponse (Maybe (NonEmpty Text))
bgwrsMissingWorkflows = lens _bgwrsMissingWorkflows (\s a -> s {_bgwrsMissingWorkflows = a}) . mapping _List1

-- | A list of workflow resource metadata.
bgwrsWorkflows :: Lens' BatchGetWorkflowsResponse (Maybe (NonEmpty Workflow))
bgwrsWorkflows = lens _bgwrsWorkflows (\s a -> s {_bgwrsWorkflows = a}) . mapping _List1

-- | -- | The response status code.
bgwrsResponseStatus :: Lens' BatchGetWorkflowsResponse Int
bgwrsResponseStatus = lens _bgwrsResponseStatus (\s a -> s {_bgwrsResponseStatus = a})

instance NFData BatchGetWorkflowsResponse
