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
-- Module      : Network.AWS.CloudDirectory.ListDirectories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists directories created within an account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDirectories
    (
    -- * Creating a Request
      listDirectories
    , ListDirectories
    -- * Request Lenses
    , ldState
    , ldNextToken
    , ldMaxResults

    -- * Destructuring the Response
    , listDirectoriesResponse
    , ListDirectoriesResponse
    -- * Response Lenses
    , ldrsNextToken
    , ldrsResponseStatus
    , ldrsDirectories
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDirectories' smart constructor.
data ListDirectories = ListDirectories'
  { _ldState      :: !(Maybe DirectoryState)
  , _ldNextToken  :: !(Maybe Text)
  , _ldMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDirectories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldState' - The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
--
-- * 'ldNextToken' - The pagination token.
--
-- * 'ldMaxResults' - The maximum number of results to retrieve.
listDirectories
    :: ListDirectories
listDirectories =
  ListDirectories'
    {_ldState = Nothing, _ldNextToken = Nothing, _ldMaxResults = Nothing}


-- | The state of the directories in the list. Can be either Enabled, Disabled, or Deleted.
ldState :: Lens' ListDirectories (Maybe DirectoryState)
ldState = lens _ldState (\ s a -> s{_ldState = a})

-- | The pagination token.
ldNextToken :: Lens' ListDirectories (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a})

-- | The maximum number of results to retrieve.
ldMaxResults :: Lens' ListDirectories (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a}) . mapping _Nat

instance AWSPager ListDirectories where
        page rq rs
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDirectories) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDirectories where
        type Rs ListDirectories = ListDirectoriesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListDirectoriesResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Directories" .!@ mempty))

instance Hashable ListDirectories where

instance NFData ListDirectories where

instance ToHeaders ListDirectories where
        toHeaders = const mempty

instance ToJSON ListDirectories where
        toJSON ListDirectories'{..}
          = object
              (catMaybes
                 [("state" .=) <$> _ldState,
                  ("NextToken" .=) <$> _ldNextToken,
                  ("MaxResults" .=) <$> _ldMaxResults])

instance ToPath ListDirectories where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/directory/list"

instance ToQuery ListDirectories where
        toQuery = const mempty

-- | /See:/ 'listDirectoriesResponse' smart constructor.
data ListDirectoriesResponse = ListDirectoriesResponse'
  { _ldrsNextToken      :: !(Maybe Text)
  , _ldrsResponseStatus :: !Int
  , _ldrsDirectories    :: ![Directory]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDirectoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - The pagination token.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
--
-- * 'ldrsDirectories' - Lists all directories that are associated with your account in pagination fashion.
listDirectoriesResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDirectoriesResponse
listDirectoriesResponse pResponseStatus_ =
  ListDirectoriesResponse'
    { _ldrsNextToken = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    , _ldrsDirectories = mempty
    }


-- | The pagination token.
ldrsNextToken :: Lens' ListDirectoriesResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDirectoriesResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

-- | Lists all directories that are associated with your account in pagination fashion.
ldrsDirectories :: Lens' ListDirectoriesResponse [Directory]
ldrsDirectories = lens _ldrsDirectories (\ s a -> s{_ldrsDirectories = a}) . _Coerce

instance NFData ListDirectoriesResponse where
