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

-- Module      : Network.AWS.MachineLearning.UpdateDataSource
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

-- | Updates the 'DataSourceName' of a 'DataSource'.
--
-- You can use the 'GetDataSource' operation to view the contents of the updated
-- data element.
--
-- <http://http://docs.aws.amazon.com/machine-learning/latest/APIReference/API_UpdateDataSource.html>
module Network.AWS.MachineLearning.UpdateDataSource
    (
    -- * Request
      UpdateDataSource
    -- ** Request constructor
    , updateDataSource
    -- ** Request lenses
    , udsDataSourceId
    , udsDataSourceName

    -- * Response
    , UpdateDataSourceResponse
    -- ** Response constructor
    , updateDataSourceResponse
    -- ** Response lenses
    , udsrDataSourceId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.MachineLearning.Types
import qualified GHC.Exts

data UpdateDataSource = UpdateDataSource
    { _udsDataSourceId   :: Text
    , _udsDataSourceName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UpdateDataSource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udsDataSourceId' @::@ 'Text'
--
-- * 'udsDataSourceName' @::@ 'Text'
--
updateDataSource :: Text -- ^ 'udsDataSourceId'
                 -> Text -- ^ 'udsDataSourceName'
                 -> UpdateDataSource
updateDataSource p1 p2 = UpdateDataSource
    { _udsDataSourceId   = p1
    , _udsDataSourceName = p2
    }

-- | The ID assigned to the 'DataSource' during creation.
udsDataSourceId :: Lens' UpdateDataSource Text
udsDataSourceId = lens _udsDataSourceId (\s a -> s { _udsDataSourceId = a })

-- | A new user-supplied name or description of the 'DataSource' that will replace
-- the current description.
udsDataSourceName :: Lens' UpdateDataSource Text
udsDataSourceName =
    lens _udsDataSourceName (\s a -> s { _udsDataSourceName = a })

newtype UpdateDataSourceResponse = UpdateDataSourceResponse
    { _udsrDataSourceId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UpdateDataSourceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udsrDataSourceId' @::@ 'Maybe' 'Text'
--
updateDataSourceResponse :: UpdateDataSourceResponse
updateDataSourceResponse = UpdateDataSourceResponse
    { _udsrDataSourceId = Nothing
    }

-- | The ID assigned to the 'DataSource' during creation. This value should be
-- identical to the value of the 'DataSourceID' in the request.
udsrDataSourceId :: Lens' UpdateDataSourceResponse (Maybe Text)
udsrDataSourceId = lens _udsrDataSourceId (\s a -> s { _udsrDataSourceId = a })

instance ToPath UpdateDataSource where
    toPath = const "/"

instance ToQuery UpdateDataSource where
    toQuery = const mempty

instance ToHeaders UpdateDataSource

instance ToJSON UpdateDataSource where
    toJSON UpdateDataSource{..} = object
        [ "DataSourceId"   .= _udsDataSourceId
        , "DataSourceName" .= _udsDataSourceName
        ]

instance AWSRequest UpdateDataSource where
    type Sv UpdateDataSource = MachineLearning
    type Rs UpdateDataSource = UpdateDataSourceResponse

    request  = post "UpdateDataSource"
    response = jsonResponse

instance FromJSON UpdateDataSourceResponse where
    parseJSON = withObject "UpdateDataSourceResponse" $ \o -> UpdateDataSourceResponse
        <$> o .:? "DataSourceId"
