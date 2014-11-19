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

-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteStreamingDistribution.html>
module Network.AWS.CloudFront.DeleteStreamingDistribution
    (
    -- * Request
      DeleteStreamingDistribution
    -- ** Request constructor
    , deleteStreamingDistribution
    -- ** Request lenses
    , dsdId
    , dsdIfMatch

    -- * Response
    , DeleteStreamingDistributionResponse
    -- ** Response constructor
    , deleteStreamingDistributionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data DeleteStreamingDistribution = DeleteStreamingDistribution
    { _dsdId      :: Text
    , _dsdIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteStreamingDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdId' @::@ 'Text'
--
-- * 'dsdIfMatch' @::@ 'Maybe' 'Text'
--
deleteStreamingDistribution :: Text -- ^ 'dsdId'
                            -> DeleteStreamingDistribution
deleteStreamingDistribution p1 = DeleteStreamingDistribution
    { _dsdId      = p1
    , _dsdIfMatch = Nothing
    }

-- | The distribution id.
dsdId :: Lens' DeleteStreamingDistribution Text
dsdId = lens _dsdId (\s a -> s { _dsdId = a })

-- | The value of the ETag header you received when you disabled the streaming
-- distribution. For example: E2QWRUHAPOMQZL.
dsdIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdIfMatch = lens _dsdIfMatch (\s a -> s { _dsdIfMatch = a })

data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStreamingDistributionResponse' constructor.
deleteStreamingDistributionResponse :: DeleteStreamingDistributionResponse
deleteStreamingDistributionResponse = DeleteStreamingDistributionResponse

instance ToPath DeleteStreamingDistribution where
    toPath DeleteStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _dsdId
        ]

instance ToQuery DeleteStreamingDistribution where
    toQuery = const mempty

instance ToHeaders DeleteStreamingDistribution where
    toHeaders DeleteStreamingDistribution{..} = mconcat
        [ "If-Match" =: _dsdIfMatch
        ]

instance ToXMLRoot DeleteStreamingDistribution where
    toXMLRoot = const (element "DeleteStreamingDistribution" [])

instance ToXML DeleteStreamingDistribution

instance AWSRequest DeleteStreamingDistribution where
    type Sv DeleteStreamingDistribution = CloudFront
    type Rs DeleteStreamingDistribution = DeleteStreamingDistributionResponse

    request  = delete
    response = nullResponse DeleteStreamingDistributionResponse
