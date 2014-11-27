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

-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
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

-- | Deletes a metric filter associated with the specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteMetricFilter.html>
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
    (
    -- * Request
      DeleteMetricFilter
    -- ** Request constructor
    , deleteMetricFilter
    -- ** Request lenses
    , dmf1FilterName
    , dmf1LogGroupName

    -- * Response
    , DeleteMetricFilterResponse
    -- ** Response constructor
    , deleteMetricFilterResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data DeleteMetricFilter = DeleteMetricFilter
    { _dmf1FilterName   :: Text
    , _dmf1LogGroupName :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteMetricFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmf1FilterName' @::@ 'Text'
--
-- * 'dmf1LogGroupName' @::@ 'Text'
--
deleteMetricFilter :: Text -- ^ 'dmf1LogGroupName'
                   -> Text -- ^ 'dmf1FilterName'
                   -> DeleteMetricFilter
deleteMetricFilter p1 p2 = DeleteMetricFilter
    { _dmf1LogGroupName = p1
    , _dmf1FilterName   = p2
    }

dmf1FilterName :: Lens' DeleteMetricFilter Text
dmf1FilterName = lens _dmf1FilterName (\s a -> s { _dmf1FilterName = a })

dmf1LogGroupName :: Lens' DeleteMetricFilter Text
dmf1LogGroupName = lens _dmf1LogGroupName (\s a -> s { _dmf1LogGroupName = a })

data DeleteMetricFilterResponse = DeleteMetricFilterResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteMetricFilterResponse' constructor.
deleteMetricFilterResponse :: DeleteMetricFilterResponse
deleteMetricFilterResponse = DeleteMetricFilterResponse

instance ToPath DeleteMetricFilter where
    toPath = const "/"

instance ToQuery DeleteMetricFilter where
    toQuery = const mempty

instance ToHeaders DeleteMetricFilter

instance ToJSON DeleteMetricFilter where
    toJSON DeleteMetricFilter{..} = object
        [ "logGroupName" .= _dmf1LogGroupName
        , "filterName"   .= _dmf1FilterName
        ]

instance AWSRequest DeleteMetricFilter where
    type Sv DeleteMetricFilter = CloudWatchLogs
    type Rs DeleteMetricFilter = DeleteMetricFilterResponse

    request  = post "DeleteMetricFilter"
    response = nullResponse DeleteMetricFilterResponse
