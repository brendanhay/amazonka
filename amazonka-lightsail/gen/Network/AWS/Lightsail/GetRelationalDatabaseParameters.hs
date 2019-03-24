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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the runtime parameters offered by the underlying database software, or engine, for a specific database in Amazon Lightsail.
--
--
-- In addition to the parameter names and values, this operation returns other information about each parameter. This information includes whether changes require a reboot, whether the parameter is modifiable, the allowed values, and the data types.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseParameters
    (
    -- * Creating a Request
      getRelationalDatabaseParameters
    , GetRelationalDatabaseParameters
    -- * Request Lenses
    , grdpPageToken
    , grdpRelationalDatabaseName

    -- * Destructuring the Response
    , getRelationalDatabaseParametersResponse
    , GetRelationalDatabaseParametersResponse
    -- * Response Lenses
    , grdprsNextPageToken
    , grdprsParameters
    , grdprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseParameters' smart constructor.
data GetRelationalDatabaseParameters = GetRelationalDatabaseParameters'
  { _grdpPageToken              :: !(Maybe Text)
  , _grdpRelationalDatabaseName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdpPageToken' - A token used for advancing to a specific page of results for your @get relational database parameters@ request.
--
-- * 'grdpRelationalDatabaseName' - The name of your database for which to get parameters.
getRelationalDatabaseParameters
    :: Text -- ^ 'grdpRelationalDatabaseName'
    -> GetRelationalDatabaseParameters
getRelationalDatabaseParameters pRelationalDatabaseName_ =
  GetRelationalDatabaseParameters'
    { _grdpPageToken = Nothing
    , _grdpRelationalDatabaseName = pRelationalDatabaseName_
    }


-- | A token used for advancing to a specific page of results for your @get relational database parameters@ request.
grdpPageToken :: Lens' GetRelationalDatabaseParameters (Maybe Text)
grdpPageToken = lens _grdpPageToken (\ s a -> s{_grdpPageToken = a})

-- | The name of your database for which to get parameters.
grdpRelationalDatabaseName :: Lens' GetRelationalDatabaseParameters Text
grdpRelationalDatabaseName = lens _grdpRelationalDatabaseName (\ s a -> s{_grdpRelationalDatabaseName = a})

instance AWSPager GetRelationalDatabaseParameters
         where
        page rq rs
          | stop (rs ^. grdprsNextPageToken) = Nothing
          | stop (rs ^. grdprsParameters) = Nothing
          | otherwise =
            Just $ rq &
              grdpPageToken .~ rs ^. grdprsNextPageToken

instance AWSRequest GetRelationalDatabaseParameters
         where
        type Rs GetRelationalDatabaseParameters =
             GetRelationalDatabaseParametersResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseParametersResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "parameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseParameters
         where

instance NFData GetRelationalDatabaseParameters where

instance ToHeaders GetRelationalDatabaseParameters
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseParameters"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseParameters where
        toJSON GetRelationalDatabaseParameters'{..}
          = object
              (catMaybes
                 [("pageToken" .=) <$> _grdpPageToken,
                  Just
                    ("relationalDatabaseName" .=
                       _grdpRelationalDatabaseName)])

instance ToPath GetRelationalDatabaseParameters where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseParameters
         where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseParametersResponse' smart constructor.
data GetRelationalDatabaseParametersResponse = GetRelationalDatabaseParametersResponse'
  { _grdprsNextPageToken  :: !(Maybe Text)
  , _grdprsParameters     :: !(Maybe [RelationalDatabaseParameter])
  , _grdprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdprsNextPageToken' - A token used for advancing to the next page of results from your get static IPs request.
--
-- * 'grdprsParameters' - An object describing the result of your get relational database parameters request.
--
-- * 'grdprsResponseStatus' - -- | The response status code.
getRelationalDatabaseParametersResponse
    :: Int -- ^ 'grdprsResponseStatus'
    -> GetRelationalDatabaseParametersResponse
getRelationalDatabaseParametersResponse pResponseStatus_ =
  GetRelationalDatabaseParametersResponse'
    { _grdprsNextPageToken = Nothing
    , _grdprsParameters = Nothing
    , _grdprsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results from your get static IPs request.
grdprsNextPageToken :: Lens' GetRelationalDatabaseParametersResponse (Maybe Text)
grdprsNextPageToken = lens _grdprsNextPageToken (\ s a -> s{_grdprsNextPageToken = a})

-- | An object describing the result of your get relational database parameters request.
grdprsParameters :: Lens' GetRelationalDatabaseParametersResponse [RelationalDatabaseParameter]
grdprsParameters = lens _grdprsParameters (\ s a -> s{_grdprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
grdprsResponseStatus :: Lens' GetRelationalDatabaseParametersResponse Int
grdprsResponseStatus = lens _grdprsResponseStatus (\ s a -> s{_grdprsResponseStatus = a})

instance NFData
           GetRelationalDatabaseParametersResponse
         where
