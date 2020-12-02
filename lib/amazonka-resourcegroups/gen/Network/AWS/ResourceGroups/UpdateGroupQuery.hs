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
-- Module      : Network.AWS.ResourceGroups.UpdateGroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource query of a group.
module Network.AWS.ResourceGroups.UpdateGroupQuery
  ( -- * Creating a Request
    updateGroupQuery,
    UpdateGroupQuery,

    -- * Request Lenses
    ugqGroup,
    ugqGroupName,
    ugqResourceQuery,

    -- * Destructuring the Response
    updateGroupQueryResponse,
    UpdateGroupQueryResponse,

    -- * Response Lenses
    ugqrsGroupQuery,
    ugqrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'updateGroupQuery' smart constructor.
data UpdateGroupQuery = UpdateGroupQuery'
  { _ugqGroup ::
      !(Maybe Text),
    _ugqGroupName :: !(Maybe Text),
    _ugqResourceQuery :: !ResourceQuery
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGroupQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugqGroup' - The name or the ARN of the resource group to query.
--
-- * 'ugqGroupName' - Don't use this parameter. Use @Group@ instead.
--
-- * 'ugqResourceQuery' - The resource query to determine which AWS resources are members of this resource group.
updateGroupQuery ::
  -- | 'ugqResourceQuery'
  ResourceQuery ->
  UpdateGroupQuery
updateGroupQuery pResourceQuery_ =
  UpdateGroupQuery'
    { _ugqGroup = Nothing,
      _ugqGroupName = Nothing,
      _ugqResourceQuery = pResourceQuery_
    }

-- | The name or the ARN of the resource group to query.
ugqGroup :: Lens' UpdateGroupQuery (Maybe Text)
ugqGroup = lens _ugqGroup (\s a -> s {_ugqGroup = a})

-- | Don't use this parameter. Use @Group@ instead.
ugqGroupName :: Lens' UpdateGroupQuery (Maybe Text)
ugqGroupName = lens _ugqGroupName (\s a -> s {_ugqGroupName = a})

-- | The resource query to determine which AWS resources are members of this resource group.
ugqResourceQuery :: Lens' UpdateGroupQuery ResourceQuery
ugqResourceQuery = lens _ugqResourceQuery (\s a -> s {_ugqResourceQuery = a})

instance AWSRequest UpdateGroupQuery where
  type Rs UpdateGroupQuery = UpdateGroupQueryResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          UpdateGroupQueryResponse'
            <$> (x .?> "GroupQuery") <*> (pure (fromEnum s))
      )

instance Hashable UpdateGroupQuery

instance NFData UpdateGroupQuery

instance ToHeaders UpdateGroupQuery where
  toHeaders = const mempty

instance ToJSON UpdateGroupQuery where
  toJSON UpdateGroupQuery' {..} =
    object
      ( catMaybes
          [ ("Group" .=) <$> _ugqGroup,
            ("GroupName" .=) <$> _ugqGroupName,
            Just ("ResourceQuery" .= _ugqResourceQuery)
          ]
      )

instance ToPath UpdateGroupQuery where
  toPath = const "/update-group-query"

instance ToQuery UpdateGroupQuery where
  toQuery = const mempty

-- | /See:/ 'updateGroupQueryResponse' smart constructor.
data UpdateGroupQueryResponse = UpdateGroupQueryResponse'
  { _ugqrsGroupQuery ::
      !(Maybe GroupQuery),
    _ugqrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGroupQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugqrsGroupQuery' - The updated resource query associated with the resource group after the update.
--
-- * 'ugqrsResponseStatus' - -- | The response status code.
updateGroupQueryResponse ::
  -- | 'ugqrsResponseStatus'
  Int ->
  UpdateGroupQueryResponse
updateGroupQueryResponse pResponseStatus_ =
  UpdateGroupQueryResponse'
    { _ugqrsGroupQuery = Nothing,
      _ugqrsResponseStatus = pResponseStatus_
    }

-- | The updated resource query associated with the resource group after the update.
ugqrsGroupQuery :: Lens' UpdateGroupQueryResponse (Maybe GroupQuery)
ugqrsGroupQuery = lens _ugqrsGroupQuery (\s a -> s {_ugqrsGroupQuery = a})

-- | -- | The response status code.
ugqrsResponseStatus :: Lens' UpdateGroupQueryResponse Int
ugqrsResponseStatus = lens _ugqrsResponseStatus (\s a -> s {_ugqrsResponseStatus = a})

instance NFData UpdateGroupQueryResponse
