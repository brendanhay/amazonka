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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available database blueprints in Amazon Lightsail. A blueprint describes the major engine version of a database.
--
--
-- You can use a blueprint ID to create a new database that runs a specific database engine.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
    (
    -- * Creating a Request
      getRelationalDatabaseBlueprints
    , GetRelationalDatabaseBlueprints
    -- * Request Lenses
    , grdbPageToken

    -- * Destructuring the Response
    , getRelationalDatabaseBlueprintsResponse
    , GetRelationalDatabaseBlueprintsResponse
    -- * Response Lenses
    , grdbsrsBlueprints
    , grdbsrsNextPageToken
    , grdbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseBlueprints' smart constructor.
newtype GetRelationalDatabaseBlueprints = GetRelationalDatabaseBlueprints'
  { _grdbPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseBlueprints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdbPageToken' - A token used for advancing to a specific page of results for your @get relational database blueprints@ request.
getRelationalDatabaseBlueprints
    :: GetRelationalDatabaseBlueprints
getRelationalDatabaseBlueprints =
  GetRelationalDatabaseBlueprints' {_grdbPageToken = Nothing}


-- | A token used for advancing to a specific page of results for your @get relational database blueprints@ request.
grdbPageToken :: Lens' GetRelationalDatabaseBlueprints (Maybe Text)
grdbPageToken = lens _grdbPageToken (\ s a -> s{_grdbPageToken = a})

instance AWSPager GetRelationalDatabaseBlueprints
         where
        page rq rs
          | stop (rs ^. grdbsrsNextPageToken) = Nothing
          | stop (rs ^. grdbsrsBlueprints) = Nothing
          | otherwise =
            Just $ rq &
              grdbPageToken .~ rs ^. grdbsrsNextPageToken

instance AWSRequest GetRelationalDatabaseBlueprints
         where
        type Rs GetRelationalDatabaseBlueprints =
             GetRelationalDatabaseBlueprintsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseBlueprintsResponse' <$>
                   (x .?> "blueprints" .!@ mempty) <*>
                     (x .?> "nextPageToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseBlueprints
         where

instance NFData GetRelationalDatabaseBlueprints where

instance ToHeaders GetRelationalDatabaseBlueprints
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseBlueprints"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseBlueprints where
        toJSON GetRelationalDatabaseBlueprints'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _grdbPageToken])

instance ToPath GetRelationalDatabaseBlueprints where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseBlueprints
         where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseBlueprintsResponse' smart constructor.
data GetRelationalDatabaseBlueprintsResponse = GetRelationalDatabaseBlueprintsResponse'
  { _grdbsrsBlueprints     :: !(Maybe [RelationalDatabaseBlueprint])
  , _grdbsrsNextPageToken  :: !(Maybe Text)
  , _grdbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseBlueprintsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdbsrsBlueprints' - An object describing the result of your get relational database blueprints request.
--
-- * 'grdbsrsNextPageToken' - A token used for advancing to the next page of results of your get relational database blueprints request.
--
-- * 'grdbsrsResponseStatus' - -- | The response status code.
getRelationalDatabaseBlueprintsResponse
    :: Int -- ^ 'grdbsrsResponseStatus'
    -> GetRelationalDatabaseBlueprintsResponse
getRelationalDatabaseBlueprintsResponse pResponseStatus_ =
  GetRelationalDatabaseBlueprintsResponse'
    { _grdbsrsBlueprints = Nothing
    , _grdbsrsNextPageToken = Nothing
    , _grdbsrsResponseStatus = pResponseStatus_
    }


-- | An object describing the result of your get relational database blueprints request.
grdbsrsBlueprints :: Lens' GetRelationalDatabaseBlueprintsResponse [RelationalDatabaseBlueprint]
grdbsrsBlueprints = lens _grdbsrsBlueprints (\ s a -> s{_grdbsrsBlueprints = a}) . _Default . _Coerce

-- | A token used for advancing to the next page of results of your get relational database blueprints request.
grdbsrsNextPageToken :: Lens' GetRelationalDatabaseBlueprintsResponse (Maybe Text)
grdbsrsNextPageToken = lens _grdbsrsNextPageToken (\ s a -> s{_grdbsrsNextPageToken = a})

-- | -- | The response status code.
grdbsrsResponseStatus :: Lens' GetRelationalDatabaseBlueprintsResponse Int
grdbsrsResponseStatus = lens _grdbsrsResponseStatus (\ s a -> s{_grdbsrsResponseStatus = a})

instance NFData
           GetRelationalDatabaseBlueprintsResponse
         where
