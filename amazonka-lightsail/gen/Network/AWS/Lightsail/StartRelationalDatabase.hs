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
-- Module      : Network.AWS.Lightsail.StartRelationalDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific database from a stopped state in Amazon Lightsail. To restart a database, use the @reboot relational database@ operation.
--
--
-- The @start relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
--
module Network.AWS.Lightsail.StartRelationalDatabase
    (
    -- * Creating a Request
      startRelationalDatabase
    , StartRelationalDatabase
    -- * Request Lenses
    , sRelationalDatabaseName

    -- * Destructuring the Response
    , startRelationalDatabaseResponse
    , StartRelationalDatabaseResponse
    -- * Response Lenses
    , srdrsOperations
    , srdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startRelationalDatabase' smart constructor.
newtype StartRelationalDatabase = StartRelationalDatabase'
  { _sRelationalDatabaseName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartRelationalDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sRelationalDatabaseName' - The name of your database to start.
startRelationalDatabase
    :: Text -- ^ 'sRelationalDatabaseName'
    -> StartRelationalDatabase
startRelationalDatabase pRelationalDatabaseName_ =
  StartRelationalDatabase' {_sRelationalDatabaseName = pRelationalDatabaseName_}


-- | The name of your database to start.
sRelationalDatabaseName :: Lens' StartRelationalDatabase Text
sRelationalDatabaseName = lens _sRelationalDatabaseName (\ s a -> s{_sRelationalDatabaseName = a})

instance AWSRequest StartRelationalDatabase where
        type Rs StartRelationalDatabase =
             StartRelationalDatabaseResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 StartRelationalDatabaseResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StartRelationalDatabase where

instance NFData StartRelationalDatabase where

instance ToHeaders StartRelationalDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.StartRelationalDatabase" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartRelationalDatabase where
        toJSON StartRelationalDatabase'{..}
          = object
              (catMaybes
                 [Just
                    ("relationalDatabaseName" .=
                       _sRelationalDatabaseName)])

instance ToPath StartRelationalDatabase where
        toPath = const "/"

instance ToQuery StartRelationalDatabase where
        toQuery = const mempty

-- | /See:/ 'startRelationalDatabaseResponse' smart constructor.
data StartRelationalDatabaseResponse = StartRelationalDatabaseResponse'
  { _srdrsOperations     :: !(Maybe [Operation])
  , _srdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdrsOperations' - An object describing the result of your start relational database request.
--
-- * 'srdrsResponseStatus' - -- | The response status code.
startRelationalDatabaseResponse
    :: Int -- ^ 'srdrsResponseStatus'
    -> StartRelationalDatabaseResponse
startRelationalDatabaseResponse pResponseStatus_ =
  StartRelationalDatabaseResponse'
    {_srdrsOperations = Nothing, _srdrsResponseStatus = pResponseStatus_}


-- | An object describing the result of your start relational database request.
srdrsOperations :: Lens' StartRelationalDatabaseResponse [Operation]
srdrsOperations = lens _srdrsOperations (\ s a -> s{_srdrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
srdrsResponseStatus :: Lens' StartRelationalDatabaseResponse Int
srdrsResponseStatus = lens _srdrsResponseStatus (\ s a -> s{_srdrsResponseStatus = a})

instance NFData StartRelationalDatabaseResponse where
