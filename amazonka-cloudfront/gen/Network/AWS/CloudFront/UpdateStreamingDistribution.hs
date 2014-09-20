{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a streaming distribution.
module Network.AWS.CloudFront.UpdateStreamingDistribution
    (
    -- * Request
      UpdateStreamingDistribution
    -- ** Request constructor
    , updateStreamingDistribution
    -- ** Request lenses
    , usdStreamingDistributionConfig
    , usdId
    , usdIfMatch

    -- * Response
    , UpdateStreamingDistributionResponse
    -- ** Response constructor
    , updateStreamingDistributionResponse
    -- ** Response lenses
    , usdrStreamingDistribution
    , usdrETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to update a streaming distribution.
data UpdateStreamingDistribution = UpdateStreamingDistribution
    { _usdStreamingDistributionConfig :: StreamingDistributionConfig
    , _usdId :: Text
    , _usdIfMatch :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStreamingDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamingDistributionConfig ::@ @StreamingDistributionConfig@
--
-- * @Id ::@ @Text@
--
-- * @IfMatch ::@ @Maybe Text@
--
updateStreamingDistribution :: StreamingDistributionConfig -- ^ 'usdStreamingDistributionConfig'
                            -> Text -- ^ 'usdId'
                            -> UpdateStreamingDistribution
updateStreamingDistribution p1 p2 = UpdateStreamingDistribution
    { _usdStreamingDistributionConfig = p1
    , _usdId = p2
    , _usdIfMatch = Nothing
    }

-- | The streaming distribution's configuration information.
usdStreamingDistributionConfig :: Lens' UpdateStreamingDistribution StreamingDistributionConfig
usdStreamingDistributionConfig =
    lens _usdStreamingDistributionConfig
         (\s a -> s { _usdStreamingDistributionConfig = a })

-- | The streaming distribution's id.
usdId :: Lens' UpdateStreamingDistribution Text
usdId = lens _usdId (\s a -> s { _usdId = a })

-- | The value of the ETag header you received when retrieving the streaming
-- distribution's configuration. For example: E2QWRUHAPOMQZL.
usdIfMatch :: Lens' UpdateStreamingDistribution (Maybe Text)
usdIfMatch = lens _usdIfMatch (\s a -> s { _usdIfMatch = a })

instance ToPath UpdateStreamingDistribution

instance ToQuery UpdateStreamingDistribution

instance ToHeaders UpdateStreamingDistribution where
    toHeaders UpdateStreamingDistribution{..} = concat
        [ "If-Match" =: _usdIfMatch
        ]

instance ToXML UpdateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateStreamingDistributionRequest"

-- | The returned result of the corresponding request.
data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { _usdrStreamingDistribution :: Maybe StreamingDistribution
    , _usdrETag :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateStreamingDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamingDistribution ::@ @Maybe StreamingDistribution@
--
-- * @ETag ::@ @Maybe Text@
--
updateStreamingDistributionResponse :: UpdateStreamingDistributionResponse
updateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { _usdrStreamingDistribution = Nothing
    , _usdrETag = Nothing
    }

-- | The streaming distribution's information.
usdrStreamingDistribution :: Lens' UpdateStreamingDistributionResponse (Maybe StreamingDistribution)
usdrStreamingDistribution =
    lens _usdrStreamingDistribution
         (\s a -> s { _usdrStreamingDistribution = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdrETag :: Lens' UpdateStreamingDistributionResponse (Maybe Text)
usdrETag = lens _usdrETag (\s a -> s { _usdrETag = a })

instance AWSRequest UpdateStreamingDistribution where
    type Sv UpdateStreamingDistribution = CloudFront
    type Rs UpdateStreamingDistribution = UpdateStreamingDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure UpdateStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
