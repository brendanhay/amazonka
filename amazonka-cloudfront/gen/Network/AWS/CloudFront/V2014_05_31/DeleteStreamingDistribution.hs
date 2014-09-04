{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution
    (
    -- * Request
      DeleteStreamingDistribution
    -- ** Request constructor
    , mkDeleteStreamingDistributionRequest
    -- ** Request lenses
    , dsdrId
    , dsdrIfMatch

    -- * Response
    , DeleteStreamingDistributionResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStreamingDistribution' request.
mkDeleteStreamingDistributionRequest :: Text -- ^ 'dsdrId'
                                     -> DeleteStreamingDistribution
mkDeleteStreamingDistributionRequest p1 = DeleteStreamingDistribution
    { _dsdrId = p1
    , _dsdrIfMatch = Nothing
    }
{-# INLINE mkDeleteStreamingDistributionRequest #-}

data DeleteStreamingDistribution = DeleteStreamingDistribution
    { _dsdrId :: Text
      -- ^ The distribution id.
    , _dsdrIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when you disabled the
      -- streaming distribution. For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The distribution id.
dsdrId :: Lens' DeleteStreamingDistribution (Text)
dsdrId = lens _dsdrId (\s a -> s { _dsdrId = a })
{-# INLINE dsdrId #-}

-- | The value of the ETag header you received when you disabled the streaming
-- distribution. For example: E2QWRUHAPOMQZL.
dsdrIfMatch :: Lens' DeleteStreamingDistribution (Maybe Text)
dsdrIfMatch = lens _dsdrIfMatch (\s a -> s { _dsdrIfMatch = a })
{-# INLINE dsdrIfMatch #-}

instance ToPath DeleteStreamingDistribution where
    toPath DeleteStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _dsdrId
        ]

instance ToQuery DeleteStreamingDistribution

instance ToHeaders DeleteStreamingDistribution

instance ToXML DeleteStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteStreamingDistributionRequest"

data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteStreamingDistribution where
    type Sv DeleteStreamingDistribution = CloudFront
    type Rs DeleteStreamingDistribution = DeleteStreamingDistributionResponse

    request = delete
    response _ = nullaryResponse DeleteStreamingDistributionResponse
