{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteTable operation deletes a table and all of its items. After a
-- DeleteTable request, the specified table is in the DELETING state until
-- DynamoDB completes the deletion. If the table is in the ACTIVE state, you
-- can delete it. If a table is in CREATING or UPDATING states, then DynamoDB
-- returns a ResourceInUseException. If the specified table does not exist,
-- DynamoDB returns a ResourceNotFoundException. If table is already in the
-- DELETING state, no error is returned. When you delete a table, any indexes
-- on that table are also deleted. Use the DescribeTable API to check the
-- status of the table.
module Network.AWS.DynamoDB.DeleteTable
    (
    -- * Request
      DeleteTable
    -- ** Request constructor
    , deleteTable
    -- ** Request lenses
    , dtTableName

    -- * Response
    , DeleteTableResponse
    -- ** Response constructor
    , deleteTableResponse
    -- ** Response lenses
    , dtrTableDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DynamoDB.Types

newtype DeleteTable = DeleteTable
    { _dtTableName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTableName' @::@ 'Text'
--
deleteTable :: Text -- ^ 'dtTableName'
            -> DeleteTable
deleteTable p1 = DeleteTable
    { _dtTableName = p1
    }

-- | The name of the table to delete.
dtTableName :: Lens' DeleteTable Text
dtTableName = lens _dtTableName (\s a -> s { _dtTableName = a })

instance ToPath DeleteTable where
    toPath = const "/"

instance ToQuery DeleteTable where
    toQuery = const mempty

instance ToHeaders DeleteTable

instance ToBody DeleteTable where
    toBody = toBody . encode . _dtTableName

newtype DeleteTableResponse = DeleteTableResponse
    { _dtrTableDescription :: Maybe TableDescription
    } deriving (Eq, Show, Generic)

-- | 'DeleteTableResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTableDescription' @::@ 'Maybe' 'TableDescription'
--
deleteTableResponse :: DeleteTableResponse
deleteTableResponse = DeleteTableResponse
    { _dtrTableDescription = Nothing
    }

dtrTableDescription :: Lens' DeleteTableResponse (Maybe TableDescription)
dtrTableDescription =
    lens _dtrTableDescription (\s a -> s { _dtrTableDescription = a })

-- FromJSON

instance AWSRequest DeleteTable where
    type Sv DeleteTable = DynamoDB
    type Rs DeleteTable = DeleteTableResponse

    request  = post'
    response = jsonResponse $ \h o -> DeleteTableResponse
        <$> o .: "TableDescription"
