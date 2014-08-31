{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateStreamingDistribution' request.
updateStreamingDistribution :: StreamingDistributionConfig -- ^ '_usdrStreamingDistributionConfig'
                            -> Text -- ^ '_usdrId'
                            -> UpdateStreamingDistribution
updateStreamingDistribution p1 p2 = UpdateStreamingDistribution
    { _usdrStreamingDistributionConfig = p1
    , _usdrId = p2
    , _usdrIfMatch = Nothing
    }

data UpdateStreamingDistribution = UpdateStreamingDistribution
    { _usdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    , _usdrId :: Text
      -- ^ The streaming distribution's id.
    , _usdrIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received when retrieving the
      -- streaming distribution's configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

makeLenses ''UpdateStreamingDistribution

instance ToPath UpdateStreamingDistribution where
    toPath UpdateStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _usdrId
        , "/config"
        ]

instance ToQuery UpdateStreamingDistribution

instance ToHeaders UpdateStreamingDistribution where
    toHeaders UpdateStreamingDistribution{..} = concat
        [ "If-Match" =: _usdrIfMatch
        ]

instance ToXML UpdateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateStreamingDistributionRequest"

data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { _usdsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    , _usdsETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

makeLenses ''UpdateStreamingDistributionResponse

instance AWSRequest UpdateStreamingDistribution where
    type Sv UpdateStreamingDistribution = CloudFront
    type Rs UpdateStreamingDistribution = UpdateStreamingDistributionResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UpdateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
