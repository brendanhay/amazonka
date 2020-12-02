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
-- Module      : Network.AWS.CloudWatchLogs.DeleteQueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a saved CloudWatch Logs Insights query definition. A query definition contains details about a saved CloudWatch Logs Insights query.
--
--
-- Each @DeleteQueryDefinition@ operation can delete one query definition.
--
-- You must have the @logs:DeleteQueryDefinition@ permission to be able to perform this operation.
module Network.AWS.CloudWatchLogs.DeleteQueryDefinition
  ( -- * Creating a Request
    deleteQueryDefinition,
    DeleteQueryDefinition,

    -- * Request Lenses
    dqdQueryDefinitionId,

    -- * Destructuring the Response
    deleteQueryDefinitionResponse,
    DeleteQueryDefinitionResponse,

    -- * Response Lenses
    drsSuccess,
    drsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteQueryDefinition' smart constructor.
newtype DeleteQueryDefinition = DeleteQueryDefinition'
  { _dqdQueryDefinitionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteQueryDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqdQueryDefinitionId' - The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
deleteQueryDefinition ::
  -- | 'dqdQueryDefinitionId'
  Text ->
  DeleteQueryDefinition
deleteQueryDefinition pQueryDefinitionId_ =
  DeleteQueryDefinition'
    { _dqdQueryDefinitionId =
        pQueryDefinitionId_
    }

-- | The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
dqdQueryDefinitionId :: Lens' DeleteQueryDefinition Text
dqdQueryDefinitionId = lens _dqdQueryDefinitionId (\s a -> s {_dqdQueryDefinitionId = a})

instance AWSRequest DeleteQueryDefinition where
  type Rs DeleteQueryDefinition = DeleteQueryDefinitionResponse
  request = postJSON cloudWatchLogs
  response =
    receiveJSON
      ( \s h x ->
          DeleteQueryDefinitionResponse'
            <$> (x .?> "success") <*> (pure (fromEnum s))
      )

instance Hashable DeleteQueryDefinition

instance NFData DeleteQueryDefinition

instance ToHeaders DeleteQueryDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Logs_20140328.DeleteQueryDefinition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteQueryDefinition where
  toJSON DeleteQueryDefinition' {..} =
    object
      (catMaybes [Just ("queryDefinitionId" .= _dqdQueryDefinitionId)])

instance ToPath DeleteQueryDefinition where
  toPath = const "/"

instance ToQuery DeleteQueryDefinition where
  toQuery = const mempty

-- | /See:/ 'deleteQueryDefinitionResponse' smart constructor.
data DeleteQueryDefinitionResponse = DeleteQueryDefinitionResponse'
  { _drsSuccess ::
      !(Maybe Bool),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteQueryDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsSuccess' - A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteQueryDefinitionResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeleteQueryDefinitionResponse
deleteQueryDefinitionResponse pResponseStatus_ =
  DeleteQueryDefinitionResponse'
    { _drsSuccess = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
drsSuccess :: Lens' DeleteQueryDefinitionResponse (Maybe Bool)
drsSuccess = lens _drsSuccess (\s a -> s {_drsSuccess = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteQueryDefinitionResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DeleteQueryDefinitionResponse
