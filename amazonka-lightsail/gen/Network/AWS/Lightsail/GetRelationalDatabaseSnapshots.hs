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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your database snapshots in Amazon Lightsail.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
    (
    -- * Creating a Request
      getRelationalDatabaseSnapshots
    , GetRelationalDatabaseSnapshots
    -- * Request Lenses
    , grdsPageToken

    -- * Destructuring the Response
    , getRelationalDatabaseSnapshotsResponse
    , GetRelationalDatabaseSnapshotsResponse
    -- * Response Lenses
    , grdsrsNextPageToken
    , grdsrsRelationalDatabaseSnapshots
    , grdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseSnapshots' smart constructor.
newtype GetRelationalDatabaseSnapshots = GetRelationalDatabaseSnapshots'
  { _grdsPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdsPageToken' - A token used for advancing to a specific page of results for your @get relational database snapshots@ request.
getRelationalDatabaseSnapshots
    :: GetRelationalDatabaseSnapshots
getRelationalDatabaseSnapshots =
  GetRelationalDatabaseSnapshots' {_grdsPageToken = Nothing}


-- | A token used for advancing to a specific page of results for your @get relational database snapshots@ request.
grdsPageToken :: Lens' GetRelationalDatabaseSnapshots (Maybe Text)
grdsPageToken = lens _grdsPageToken (\ s a -> s{_grdsPageToken = a})

instance AWSPager GetRelationalDatabaseSnapshots
         where
        page rq rs
          | stop (rs ^. grdsrsNextPageToken) = Nothing
          | stop (rs ^. grdsrsRelationalDatabaseSnapshots) =
            Nothing
          | otherwise =
            Just $ rq &
              grdsPageToken .~ rs ^. grdsrsNextPageToken

instance AWSRequest GetRelationalDatabaseSnapshots
         where
        type Rs GetRelationalDatabaseSnapshots =
             GetRelationalDatabaseSnapshotsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseSnapshotsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "relationalDatabaseSnapshots" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseSnapshots
         where

instance NFData GetRelationalDatabaseSnapshots where

instance ToHeaders GetRelationalDatabaseSnapshots
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseSnapshots"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseSnapshots where
        toJSON GetRelationalDatabaseSnapshots'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _grdsPageToken])

instance ToPath GetRelationalDatabaseSnapshots where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseSnapshots where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseSnapshotsResponse' smart constructor.
data GetRelationalDatabaseSnapshotsResponse = GetRelationalDatabaseSnapshotsResponse'
  { _grdsrsNextPageToken               :: !(Maybe Text)
  , _grdsrsRelationalDatabaseSnapshots :: !(Maybe [RelationalDatabaseSnapshot])
  , _grdsrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdsrsNextPageToken' - A token used for advancing to the next page of results from your get relational database snapshots request.
--
-- * 'grdsrsRelationalDatabaseSnapshots' - An object describing the result of your get relational database snapshots request.
--
-- * 'grdsrsResponseStatus' - -- | The response status code.
getRelationalDatabaseSnapshotsResponse
    :: Int -- ^ 'grdsrsResponseStatus'
    -> GetRelationalDatabaseSnapshotsResponse
getRelationalDatabaseSnapshotsResponse pResponseStatus_ =
  GetRelationalDatabaseSnapshotsResponse'
    { _grdsrsNextPageToken = Nothing
    , _grdsrsRelationalDatabaseSnapshots = Nothing
    , _grdsrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get relational database snapshots request.
grdsrsNextPageToken :: Lens' GetRelationalDatabaseSnapshotsResponse (Maybe Text)
grdsrsNextPageToken = lens _grdsrsNextPageToken (\ s a -> s{_grdsrsNextPageToken = a})

-- | An object describing the result of your get relational database snapshots request.
grdsrsRelationalDatabaseSnapshots :: Lens' GetRelationalDatabaseSnapshotsResponse [RelationalDatabaseSnapshot]
grdsrsRelationalDatabaseSnapshots = lens _grdsrsRelationalDatabaseSnapshots (\ s a -> s{_grdsrsRelationalDatabaseSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
grdsrsResponseStatus :: Lens' GetRelationalDatabaseSnapshotsResponse Int
grdsrsResponseStatus = lens _grdsrsResponseStatus (\ s a -> s{_grdsrsResponseStatus = a})

instance NFData
           GetRelationalDatabaseSnapshotsResponse
         where
