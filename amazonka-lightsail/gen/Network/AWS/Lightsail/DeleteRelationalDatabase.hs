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
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database in Amazon Lightsail.
--
--
-- The @delete relational database@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
--
module Network.AWS.Lightsail.DeleteRelationalDatabase
    (
    -- * Creating a Request
      deleteRelationalDatabase
    , DeleteRelationalDatabase
    -- * Request Lenses
    , drdSkipFinalSnapshot
    , drdFinalRelationalDatabaseSnapshotName
    , drdRelationalDatabaseName

    -- * Destructuring the Response
    , deleteRelationalDatabaseResponse
    , DeleteRelationalDatabaseResponse
    -- * Response Lenses
    , drdrsOperations
    , drdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRelationalDatabase' smart constructor.
data DeleteRelationalDatabase = DeleteRelationalDatabase'
  { _drdSkipFinalSnapshot                   :: !(Maybe Bool)
  , _drdFinalRelationalDatabaseSnapshotName :: !(Maybe Text)
  , _drdRelationalDatabaseName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRelationalDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdSkipFinalSnapshot' - Determines whether a final database snapshot is created before your database is deleted. If @true@ is specified, no database snapshot is created. If @false@ is specified, a database snapshot is created before your database is deleted. You must specify the @final relational database snapshot name@ parameter if the @skip final snapshot@ parameter is @false@ . Default: @false@
--
-- * 'drdFinalRelationalDatabaseSnapshotName' - The name of the database snapshot created if @skip final snapshot@ is @false@ , which is the default value for that parameter. Constraints:     * Must contain from 2 to 255 alphanumeric characters, or hyphens.     * The first and last character must be a letter or number.
--
-- * 'drdRelationalDatabaseName' - The name of the database that you are deleting.
deleteRelationalDatabase
    :: Text -- ^ 'drdRelationalDatabaseName'
    -> DeleteRelationalDatabase
deleteRelationalDatabase pRelationalDatabaseName_ =
  DeleteRelationalDatabase'
    { _drdSkipFinalSnapshot = Nothing
    , _drdFinalRelationalDatabaseSnapshotName = Nothing
    , _drdRelationalDatabaseName = pRelationalDatabaseName_
    }


-- | Determines whether a final database snapshot is created before your database is deleted. If @true@ is specified, no database snapshot is created. If @false@ is specified, a database snapshot is created before your database is deleted. You must specify the @final relational database snapshot name@ parameter if the @skip final snapshot@ parameter is @false@ . Default: @false@
drdSkipFinalSnapshot :: Lens' DeleteRelationalDatabase (Maybe Bool)
drdSkipFinalSnapshot = lens _drdSkipFinalSnapshot (\ s a -> s{_drdSkipFinalSnapshot = a})

-- | The name of the database snapshot created if @skip final snapshot@ is @false@ , which is the default value for that parameter. Constraints:     * Must contain from 2 to 255 alphanumeric characters, or hyphens.     * The first and last character must be a letter or number.
drdFinalRelationalDatabaseSnapshotName :: Lens' DeleteRelationalDatabase (Maybe Text)
drdFinalRelationalDatabaseSnapshotName = lens _drdFinalRelationalDatabaseSnapshotName (\ s a -> s{_drdFinalRelationalDatabaseSnapshotName = a})

-- | The name of the database that you are deleting.
drdRelationalDatabaseName :: Lens' DeleteRelationalDatabase Text
drdRelationalDatabaseName = lens _drdRelationalDatabaseName (\ s a -> s{_drdRelationalDatabaseName = a})

instance AWSRequest DeleteRelationalDatabase where
        type Rs DeleteRelationalDatabase =
             DeleteRelationalDatabaseResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRelationalDatabaseResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteRelationalDatabase where

instance NFData DeleteRelationalDatabase where

instance ToHeaders DeleteRelationalDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DeleteRelationalDatabase" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRelationalDatabase where
        toJSON DeleteRelationalDatabase'{..}
          = object
              (catMaybes
                 [("skipFinalSnapshot" .=) <$> _drdSkipFinalSnapshot,
                  ("finalRelationalDatabaseSnapshotName" .=) <$>
                    _drdFinalRelationalDatabaseSnapshotName,
                  Just
                    ("relationalDatabaseName" .=
                       _drdRelationalDatabaseName)])

instance ToPath DeleteRelationalDatabase where
        toPath = const "/"

instance ToQuery DeleteRelationalDatabase where
        toQuery = const mempty

-- | /See:/ 'deleteRelationalDatabaseResponse' smart constructor.
data DeleteRelationalDatabaseResponse = DeleteRelationalDatabaseResponse'
  { _drdrsOperations     :: !(Maybe [Operation])
  , _drdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdrsOperations' - An object describing the result of your delete relational database request.
--
-- * 'drdrsResponseStatus' - -- | The response status code.
deleteRelationalDatabaseResponse
    :: Int -- ^ 'drdrsResponseStatus'
    -> DeleteRelationalDatabaseResponse
deleteRelationalDatabaseResponse pResponseStatus_ =
  DeleteRelationalDatabaseResponse'
    {_drdrsOperations = Nothing, _drdrsResponseStatus = pResponseStatus_}


-- | An object describing the result of your delete relational database request.
drdrsOperations :: Lens' DeleteRelationalDatabaseResponse [Operation]
drdrsOperations = lens _drdrsOperations (\ s a -> s{_drdrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
drdrsResponseStatus :: Lens' DeleteRelationalDatabaseResponse Int
drdrsResponseStatus = lens _drdrsResponseStatus (\ s a -> s{_drdrsResponseStatus = a})

instance NFData DeleteRelationalDatabaseResponse
         where
