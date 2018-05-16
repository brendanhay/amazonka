{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroupQuery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource query associated with the specified resource group.
--
--
module Network.AWS.ResourceGroups.GetGroupQuery
    (
    -- * Creating a Request
      getGroupQuery
    , GetGroupQuery
    -- * Request Lenses
    , ggqGroupName

    -- * Destructuring the Response
    , getGroupQueryResponse
    , GetGroupQueryResponse
    -- * Response Lenses
    , ggqrsGroupQuery
    , ggqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.ResourceGroups.Types.Product
import Network.AWS.Response

-- | /See:/ 'getGroupQuery' smart constructor.
newtype GetGroupQuery = GetGroupQuery'
  { _ggqGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggqGroupName' - The name of the resource group.
getGroupQuery
    :: Text -- ^ 'ggqGroupName'
    -> GetGroupQuery
getGroupQuery pGroupName_ = GetGroupQuery' {_ggqGroupName = pGroupName_}


-- | The name of the resource group.
ggqGroupName :: Lens' GetGroupQuery Text
ggqGroupName = lens _ggqGroupName (\ s a -> s{_ggqGroupName = a})

instance AWSRequest GetGroupQuery where
        type Rs GetGroupQuery = GetGroupQueryResponse
        request = get resourceGroups
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupQueryResponse' <$>
                   (x .?> "GroupQuery") <*> (pure (fromEnum s)))

instance Hashable GetGroupQuery where

instance NFData GetGroupQuery where

instance ToHeaders GetGroupQuery where
        toHeaders = const mempty

instance ToPath GetGroupQuery where
        toPath GetGroupQuery'{..}
          = mconcat ["/groups/", toBS _ggqGroupName, "/query"]

instance ToQuery GetGroupQuery where
        toQuery = const mempty

-- | /See:/ 'getGroupQueryResponse' smart constructor.
data GetGroupQueryResponse = GetGroupQueryResponse'
  { _ggqrsGroupQuery     :: !(Maybe GroupQuery)
  , _ggqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetGroupQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggqrsGroupQuery' - The resource query associated with the specified group.
--
-- * 'ggqrsResponseStatus' - -- | The response status code.
getGroupQueryResponse
    :: Int -- ^ 'ggqrsResponseStatus'
    -> GetGroupQueryResponse
getGroupQueryResponse pResponseStatus_ =
  GetGroupQueryResponse'
    {_ggqrsGroupQuery = Nothing, _ggqrsResponseStatus = pResponseStatus_}


-- | The resource query associated with the specified group.
ggqrsGroupQuery :: Lens' GetGroupQueryResponse (Maybe GroupQuery)
ggqrsGroupQuery = lens _ggqrsGroupQuery (\ s a -> s{_ggqrsGroupQuery = a})

-- | -- | The response status code.
ggqrsResponseStatus :: Lens' GetGroupQueryResponse Int
ggqrsResponseStatus = lens _ggqrsResponseStatus (\ s a -> s{_ggqrsResponseStatus = a})

instance NFData GetGroupQueryResponse where
