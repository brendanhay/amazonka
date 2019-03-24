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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your databases in Amazon Lightsail.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabases
    (
    -- * Creating a Request
      getRelationalDatabases
    , GetRelationalDatabases
    -- * Request Lenses
    , grdPageToken

    -- * Destructuring the Response
    , getRelationalDatabasesResponse
    , GetRelationalDatabasesResponse
    -- * Response Lenses
    , grdrsNextPageToken
    , grdrsRelationalDatabases
    , grdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabases' smart constructor.
newtype GetRelationalDatabases = GetRelationalDatabases'
  { _grdPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdPageToken' - A token used for advancing to a specific page of results for your @get relational database@ request.
getRelationalDatabases
    :: GetRelationalDatabases
getRelationalDatabases = GetRelationalDatabases' {_grdPageToken = Nothing}


-- | A token used for advancing to a specific page of results for your @get relational database@ request.
grdPageToken :: Lens' GetRelationalDatabases (Maybe Text)
grdPageToken = lens _grdPageToken (\ s a -> s{_grdPageToken = a})

instance AWSPager GetRelationalDatabases where
        page rq rs
          | stop (rs ^. grdrsNextPageToken) = Nothing
          | stop (rs ^. grdrsRelationalDatabases) = Nothing
          | otherwise =
            Just $ rq & grdPageToken .~ rs ^. grdrsNextPageToken

instance AWSRequest GetRelationalDatabases where
        type Rs GetRelationalDatabases =
             GetRelationalDatabasesResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabasesResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "relationalDatabases" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabases where

instance NFData GetRelationalDatabases where

instance ToHeaders GetRelationalDatabases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabases" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabases where
        toJSON GetRelationalDatabases'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _grdPageToken])

instance ToPath GetRelationalDatabases where
        toPath = const "/"

instance ToQuery GetRelationalDatabases where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabasesResponse' smart constructor.
data GetRelationalDatabasesResponse = GetRelationalDatabasesResponse'
  { _grdrsNextPageToken       :: !(Maybe Text)
  , _grdrsRelationalDatabases :: !(Maybe [RelationalDatabase])
  , _grdrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdrsNextPageToken' - A token used for advancing to the next page of results from your get relational databases request.
--
-- * 'grdrsRelationalDatabases' - An object describing the result of your get relational databases request.
--
-- * 'grdrsResponseStatus' - -- | The response status code.
getRelationalDatabasesResponse
    :: Int -- ^ 'grdrsResponseStatus'
    -> GetRelationalDatabasesResponse
getRelationalDatabasesResponse pResponseStatus_ =
  GetRelationalDatabasesResponse'
    { _grdrsNextPageToken = Nothing
    , _grdrsRelationalDatabases = Nothing
    , _grdrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get relational databases request.
grdrsNextPageToken :: Lens' GetRelationalDatabasesResponse (Maybe Text)
grdrsNextPageToken = lens _grdrsNextPageToken (\ s a -> s{_grdrsNextPageToken = a})

-- | An object describing the result of your get relational databases request.
grdrsRelationalDatabases :: Lens' GetRelationalDatabasesResponse [RelationalDatabase]
grdrsRelationalDatabases = lens _grdrsRelationalDatabases (\ s a -> s{_grdrsRelationalDatabases = a}) . _Default . _Coerce

-- | -- | The response status code.
grdrsResponseStatus :: Lens' GetRelationalDatabasesResponse Int
grdrsResponseStatus = lens _grdrsResponseStatus (\ s a -> s{_grdrsResponseStatus = a})

instance NFData GetRelationalDatabasesResponse where
