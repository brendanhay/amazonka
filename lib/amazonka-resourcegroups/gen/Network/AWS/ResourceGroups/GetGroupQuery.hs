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
-- Module      : Network.AWS.ResourceGroups.GetGroupQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource query associated with the specified resource group.
module Network.AWS.ResourceGroups.GetGroupQuery
  ( -- * Creating a Request
    getGroupQuery,
    GetGroupQuery,

    -- * Request Lenses
    ggqGroup,
    ggqGroupName,

    -- * Destructuring the Response
    getGroupQueryResponse,
    GetGroupQueryResponse,

    -- * Response Lenses
    ggqrsGroupQuery,
    ggqrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'getGroupQuery' smart constructor.
data GetGroupQuery = GetGroupQuery'
  { _ggqGroup :: !(Maybe Text),
    _ggqGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggqGroup' - The name or the ARN of the resource group to query.
--
-- * 'ggqGroupName' - Don't use this parameter. Use @Group@ instead.
getGroupQuery ::
  GetGroupQuery
getGroupQuery =
  GetGroupQuery' {_ggqGroup = Nothing, _ggqGroupName = Nothing}

-- | The name or the ARN of the resource group to query.
ggqGroup :: Lens' GetGroupQuery (Maybe Text)
ggqGroup = lens _ggqGroup (\s a -> s {_ggqGroup = a})

-- | Don't use this parameter. Use @Group@ instead.
ggqGroupName :: Lens' GetGroupQuery (Maybe Text)
ggqGroupName = lens _ggqGroupName (\s a -> s {_ggqGroupName = a})

instance AWSRequest GetGroupQuery where
  type Rs GetGroupQuery = GetGroupQueryResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          GetGroupQueryResponse'
            <$> (x .?> "GroupQuery") <*> (pure (fromEnum s))
      )

instance Hashable GetGroupQuery

instance NFData GetGroupQuery

instance ToHeaders GetGroupQuery where
  toHeaders = const mempty

instance ToJSON GetGroupQuery where
  toJSON GetGroupQuery' {..} =
    object
      ( catMaybes
          [("Group" .=) <$> _ggqGroup, ("GroupName" .=) <$> _ggqGroupName]
      )

instance ToPath GetGroupQuery where
  toPath = const "/get-group-query"

instance ToQuery GetGroupQuery where
  toQuery = const mempty

-- | /See:/ 'getGroupQueryResponse' smart constructor.
data GetGroupQueryResponse = GetGroupQueryResponse'
  { _ggqrsGroupQuery ::
      !(Maybe GroupQuery),
    _ggqrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggqrsGroupQuery' - The resource query associated with the specified group.
--
-- * 'ggqrsResponseStatus' - -- | The response status code.
getGroupQueryResponse ::
  -- | 'ggqrsResponseStatus'
  Int ->
  GetGroupQueryResponse
getGroupQueryResponse pResponseStatus_ =
  GetGroupQueryResponse'
    { _ggqrsGroupQuery = Nothing,
      _ggqrsResponseStatus = pResponseStatus_
    }

-- | The resource query associated with the specified group.
ggqrsGroupQuery :: Lens' GetGroupQueryResponse (Maybe GroupQuery)
ggqrsGroupQuery = lens _ggqrsGroupQuery (\s a -> s {_ggqrsGroupQuery = a})

-- | -- | The response status code.
ggqrsResponseStatus :: Lens' GetGroupQueryResponse Int
ggqrsResponseStatus = lens _ggqrsResponseStatus (\s a -> s {_ggqrsResponseStatus = a})

instance NFData GetGroupQueryResponse
