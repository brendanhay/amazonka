{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfos
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionInfos where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.WorkflowExecutionInfo

-- | Contains a paginated list of information about workflow executions.
--
--
--
-- /See:/ 'workflowExecutionInfos' smart constructor.
data WorkflowExecutionInfos = WorkflowExecutionInfos'
  { _weiNextPageToken ::
      !(Maybe Text),
    _weiExecutionInfos ::
      ![WorkflowExecutionInfo]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowExecutionInfos' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'weiNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'weiExecutionInfos' - The list of workflow information structures.
workflowExecutionInfos ::
  WorkflowExecutionInfos
workflowExecutionInfos =
  WorkflowExecutionInfos'
    { _weiNextPageToken = Nothing,
      _weiExecutionInfos = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
weiNextPageToken :: Lens' WorkflowExecutionInfos (Maybe Text)
weiNextPageToken = lens _weiNextPageToken (\s a -> s {_weiNextPageToken = a})

-- | The list of workflow information structures.
weiExecutionInfos :: Lens' WorkflowExecutionInfos [WorkflowExecutionInfo]
weiExecutionInfos = lens _weiExecutionInfos (\s a -> s {_weiExecutionInfos = a}) . _Coerce

instance FromJSON WorkflowExecutionInfos where
  parseJSON =
    withObject
      "WorkflowExecutionInfos"
      ( \x ->
          WorkflowExecutionInfos'
            <$> (x .:? "nextPageToken") <*> (x .:? "executionInfos" .!= mempty)
      )

instance Hashable WorkflowExecutionInfos

instance NFData WorkflowExecutionInfos
