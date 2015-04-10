{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Assigns the DELETED status to a 'DataSource', rendering it unusable.
--
-- After using the 'DeleteDataSource' operation, you can use the 'GetDataSource'
-- operation to verify that the status of the 'DataSource' changed to DELETED.
--
-- Caution The results of the 'DeleteDataSource' operation are irreversible.
--
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_DeleteDataSource.html>
module Network.AWS.MachineLearning.DeleteDataSource
    (
    -- * Request
      DeleteDataSource
    -- ** Request constructor
    , deleteDataSource
    -- ** Request lenses
    , ddsDataSourceId

    -- * Response
    , DeleteDataSourceResponse
    -- ** Response constructor
    , deleteDataSourceResponse
    -- ** Response lenses
    , ddsrDataSourceId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

newtype DeleteDataSource = DeleteDataSource
    { _ddsDataSourceId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteDataSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsDataSourceId' @::@ 'Text'
--
deleteDataSource :: Text -- ^ 'ddsDataSourceId'
                 -> DeleteDataSource
deleteDataSource p1 = DeleteDataSource
    { _ddsDataSourceId = p1
    }

-- | A user-supplied ID that uniquely identifies the 'DataSource'.
ddsDataSourceId :: Lens' DeleteDataSource Text
ddsDataSourceId = lens _ddsDataSourceId (\s a -> s { _ddsDataSourceId = a })

newtype DeleteDataSourceResponse = DeleteDataSourceResponse
    { _ddsrDataSourceId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DeleteDataSourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsrDataSourceId' @::@ 'Maybe' 'Text'
--
deleteDataSourceResponse :: DeleteDataSourceResponse
deleteDataSourceResponse = DeleteDataSourceResponse
    { _ddsrDataSourceId = Nothing
    }

-- | A user-supplied ID that uniquely identifies the 'DataSource'. This value should
-- be identical to the value of the 'DataSourceID' in the request.
ddsrDataSourceId :: Lens' DeleteDataSourceResponse (Maybe Text)
ddsrDataSourceId = lens _ddsrDataSourceId (\s a -> s { _ddsrDataSourceId = a })

instance ToPath DeleteDataSource where
    toPath = const "/"

instance ToQuery DeleteDataSource where
    toQuery = const mempty

instance ToHeaders DeleteDataSource

instance ToJSON DeleteDataSource where
    toJSON DeleteDataSource{..} = object
        [ "DataSourceId" .= _ddsDataSourceId
        ]

instance AWSRequest DeleteDataSource where
    type Sv DeleteDataSource = MachineLearning
    type Rs DeleteDataSource = DeleteDataSourceResponse

    request  = post "DeleteDataSource"
    response = jsonResponse

instance FromJSON DeleteDataSourceResponse where
    parseJSON = withObject "DeleteDataSourceResponse" $ \o -> DeleteDataSourceResponse
        <$> o .:? "DataSourceId"
