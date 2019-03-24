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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available log streams for a specific database in Amazon Lightsail.
--
--
module Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
    (
    -- * Creating a Request
      getRelationalDatabaseLogStreams
    , GetRelationalDatabaseLogStreams
    -- * Request Lenses
    , grdlsRelationalDatabaseName

    -- * Destructuring the Response
    , getRelationalDatabaseLogStreamsResponse
    , GetRelationalDatabaseLogStreamsResponse
    -- * Response Lenses
    , grdlsrsLogStreams
    , grdlsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseLogStreams' smart constructor.
newtype GetRelationalDatabaseLogStreams = GetRelationalDatabaseLogStreams'
  { _grdlsRelationalDatabaseName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseLogStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdlsRelationalDatabaseName' - The name of your database for which to get log streams.
getRelationalDatabaseLogStreams
    :: Text -- ^ 'grdlsRelationalDatabaseName'
    -> GetRelationalDatabaseLogStreams
getRelationalDatabaseLogStreams pRelationalDatabaseName_ =
  GetRelationalDatabaseLogStreams'
    {_grdlsRelationalDatabaseName = pRelationalDatabaseName_}


-- | The name of your database for which to get log streams.
grdlsRelationalDatabaseName :: Lens' GetRelationalDatabaseLogStreams Text
grdlsRelationalDatabaseName = lens _grdlsRelationalDatabaseName (\ s a -> s{_grdlsRelationalDatabaseName = a})

instance AWSRequest GetRelationalDatabaseLogStreams
         where
        type Rs GetRelationalDatabaseLogStreams =
             GetRelationalDatabaseLogStreamsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseLogStreamsResponse' <$>
                   (x .?> "logStreams" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable GetRelationalDatabaseLogStreams
         where

instance NFData GetRelationalDatabaseLogStreams where

instance ToHeaders GetRelationalDatabaseLogStreams
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabaseLogStreams"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabaseLogStreams where
        toJSON GetRelationalDatabaseLogStreams'{..}
          = object
              (catMaybes
                 [Just
                    ("relationalDatabaseName" .=
                       _grdlsRelationalDatabaseName)])

instance ToPath GetRelationalDatabaseLogStreams where
        toPath = const "/"

instance ToQuery GetRelationalDatabaseLogStreams
         where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseLogStreamsResponse' smart constructor.
data GetRelationalDatabaseLogStreamsResponse = GetRelationalDatabaseLogStreamsResponse'
  { _grdlsrsLogStreams     :: !(Maybe [Text])
  , _grdlsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRelationalDatabaseLogStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdlsrsLogStreams' - An object describing the result of your get relational database log streams request.
--
-- * 'grdlsrsResponseStatus' - -- | The response status code.
getRelationalDatabaseLogStreamsResponse
    :: Int -- ^ 'grdlsrsResponseStatus'
    -> GetRelationalDatabaseLogStreamsResponse
getRelationalDatabaseLogStreamsResponse pResponseStatus_ =
  GetRelationalDatabaseLogStreamsResponse'
    {_grdlsrsLogStreams = Nothing, _grdlsrsResponseStatus = pResponseStatus_}


-- | An object describing the result of your get relational database log streams request.
grdlsrsLogStreams :: Lens' GetRelationalDatabaseLogStreamsResponse [Text]
grdlsrsLogStreams = lens _grdlsrsLogStreams (\ s a -> s{_grdlsrsLogStreams = a}) . _Default . _Coerce

-- | -- | The response status code.
grdlsrsResponseStatus :: Lens' GetRelationalDatabaseLogStreamsResponse Int
grdlsrsResponseStatus = lens _grdlsrsResponseStatus (\ s a -> s{_grdlsrsResponseStatus = a})

instance NFData
           GetRelationalDatabaseLogStreamsResponse
         where
