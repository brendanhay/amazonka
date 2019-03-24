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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBundles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available in Amazon Lightsail. A bundle describes the performance specifications for a database.
--
--
-- You can use a bundle ID to create a new database with explicit performance specifications.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBundles
    (
    -- * Creating a Request
      getRelationalDatabaseBundles
    , GetRelationalDatabaseBundles
    -- * Request Lenses
    , gPageToken

    -- * Destructuring the Response
    , getRelationalDatabaseBundlesResponse
    , GetRelationalDatabaseBundlesResponse
    -- * Response Lenses
    , grdbrsNextPageToken
    , grdbrsBundles
    , grdbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseBundles' smart constructor.
newtype GetRelationalDatabaseBundles = GetRelationalDatabaseBundles'
  { _gPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseBundles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPageToken' - A token used for advancing to a specific page of results for your @get relational database bundles@ request.
getRelationalDatabaseBundles
    :: GetRelationalDatabaseBundles
getRelationalDatabaseBundles =
  GetRelationalDatabaseBundles' {_gPageToken = Nothing}


-- | A token used for advancing to a specific page of results for your @get relational database bundles@ request.
gPageToken :: Lens' GetRelationalDatabaseBundles (Maybe Text)
gPageToken = lens _gPageToken (\ s a -> s{_gPageToken = a})

instance AWSPager GetRelationalDatabaseBundles where
        page rq rs
          | stop (rs ^. grdbrsNextPageToken) = Nothing
          | stop (rs ^. grdbrsBundles) = Nothing
          | otherwise =
            Just $ rq & gPageToken .~ rs ^. grdbrsNextPageToken

instance AWSRequest GetRelationalDatabaseBundles
         where
        type Rs GetRelationalDatabaseBundles =
             GetRelationalDatabaseBundlesResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseBundlesResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "bundles" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseBundles where

instance NFData GetRelationalDatabaseBundles where

instance ToHeaders GetRelationalDatabaseBundles where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseBundles" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseBundles where
        toJSON GetRelationalDatabaseBundles'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gPageToken])

instance ToPath GetRelationalDatabaseBundles where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseBundles where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseBundlesResponse' smart constructor.
data GetRelationalDatabaseBundlesResponse = GetRelationalDatabaseBundlesResponse'
  { _grdbrsNextPageToken  :: !(Maybe Text)
  , _grdbrsBundles        :: !(Maybe [RelationalDatabaseBundle])
  , _grdbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseBundlesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdbrsNextPageToken' - A token used for advancing to the next page of results of your get relational database bundles request.
--
-- * 'grdbrsBundles' - An object describing the result of your get relational database bundles request.
--
-- * 'grdbrsResponseStatus' - -- | The response status code.
getRelationalDatabaseBundlesResponse
    :: Int -- ^ 'grdbrsResponseStatus'
    -> GetRelationalDatabaseBundlesResponse
getRelationalDatabaseBundlesResponse pResponseStatus_ =
  GetRelationalDatabaseBundlesResponse'
    { _grdbrsNextPageToken = Nothing
    , _grdbrsBundles = Nothing
    , _grdbrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results of your get relational database bundles request.
grdbrsNextPageToken :: Lens' GetRelationalDatabaseBundlesResponse (Maybe Text)
grdbrsNextPageToken = lens _grdbrsNextPageToken (\ s a -> s{_grdbrsNextPageToken = a})

-- | An object describing the result of your get relational database bundles request.
grdbrsBundles :: Lens' GetRelationalDatabaseBundlesResponse [RelationalDatabaseBundle]
grdbrsBundles = lens _grdbrsBundles (\ s a -> s{_grdbrsBundles = a}) . _Default . _Coerce

-- | -- | The response status code.
grdbrsResponseStatus :: Lens' GetRelationalDatabaseBundlesResponse Int
grdbrsResponseStatus = lens _grdbrsResponseStatus (\ s a -> s{_grdbrsResponseStatus = a})

instance NFData GetRelationalDatabaseBundlesResponse
         where
