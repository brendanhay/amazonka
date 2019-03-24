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
-- Module      : Network.AWS.Lightsail.StopRelationalDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specific database that is currently running in Amazon Lightsail.
--
--
-- The @stop relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
--
module Network.AWS.Lightsail.StopRelationalDatabase
    (
    -- * Creating a Request
      stopRelationalDatabase
    , StopRelationalDatabase
    -- * Request Lenses
    , srdRelationalDatabaseSnapshotName
    , srdRelationalDatabaseName

    -- * Destructuring the Response
    , stopRelationalDatabaseResponse
    , StopRelationalDatabaseResponse
    -- * Response Lenses
    , storsOperations
    , storsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopRelationalDatabase' smart constructor.
data StopRelationalDatabase = StopRelationalDatabase'
  { _srdRelationalDatabaseSnapshotName :: !(Maybe Text)
  , _srdRelationalDatabaseName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopRelationalDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdRelationalDatabaseSnapshotName' - The name of your new database snapshot to be created before stopping your database.
--
-- * 'srdRelationalDatabaseName' - The name of your database to stop.
stopRelationalDatabase
    :: Text -- ^ 'srdRelationalDatabaseName'
    -> StopRelationalDatabase
stopRelationalDatabase pRelationalDatabaseName_ =
  StopRelationalDatabase'
    { _srdRelationalDatabaseSnapshotName = Nothing
    , _srdRelationalDatabaseName = pRelationalDatabaseName_
    }


-- | The name of your new database snapshot to be created before stopping your database.
srdRelationalDatabaseSnapshotName :: Lens' StopRelationalDatabase (Maybe Text)
srdRelationalDatabaseSnapshotName = lens _srdRelationalDatabaseSnapshotName (\ s a -> s{_srdRelationalDatabaseSnapshotName = a})

-- | The name of your database to stop.
srdRelationalDatabaseName :: Lens' StopRelationalDatabase Text
srdRelationalDatabaseName = lens _srdRelationalDatabaseName (\ s a -> s{_srdRelationalDatabaseName = a})

instance AWSRequest StopRelationalDatabase where
        type Rs StopRelationalDatabase =
             StopRelationalDatabaseResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 StopRelationalDatabaseResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StopRelationalDatabase where

instance NFData StopRelationalDatabase where

instance ToHeaders StopRelationalDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.StopRelationalDatabase" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopRelationalDatabase where
        toJSON StopRelationalDatabase'{..}
          = object
              (catMaybes
                 [("relationalDatabaseSnapshotName" .=) <$>
                    _srdRelationalDatabaseSnapshotName,
                  Just
                    ("relationalDatabaseName" .=
                       _srdRelationalDatabaseName)])

instance ToPath StopRelationalDatabase where
        toPath = const "/"

instance ToQuery StopRelationalDatabase where
        toQuery = const mempty

-- | /See:/ 'stopRelationalDatabaseResponse' smart constructor.
data StopRelationalDatabaseResponse = StopRelationalDatabaseResponse'
  { _storsOperations     :: !(Maybe [Operation])
  , _storsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'storsOperations' - An object describing the result of your stop relational database request.
--
-- * 'storsResponseStatus' - -- | The response status code.
stopRelationalDatabaseResponse
    :: Int -- ^ 'storsResponseStatus'
    -> StopRelationalDatabaseResponse
stopRelationalDatabaseResponse pResponseStatus_ =
  StopRelationalDatabaseResponse'
    {_storsOperations = Nothing, _storsResponseStatus = pResponseStatus_}


-- | An object describing the result of your stop relational database request.
storsOperations :: Lens' StopRelationalDatabaseResponse [Operation]
storsOperations = lens _storsOperations (\ s a -> s{_storsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
storsResponseStatus :: Lens' StopRelationalDatabaseResponse Int
storsResponseStatus = lens _storsResponseStatus (\ s a -> s{_storsResponseStatus = a})

instance NFData StopRelationalDatabaseResponse where
