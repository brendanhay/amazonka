{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a streaming distribution.
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Request
      GetStreamingDistribution
    -- ** Request constructor
    , getStreamingDistribution
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistributionResponse
    -- ** Response constructor
    , getStreamingDistributionResponse
    -- ** Response lenses
    , gsdrStreamingDistribution
    , gsdrETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get a streaming distribution's information.
newtype GetStreamingDistribution = GetStreamingDistribution
    { _gsdId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStreamingDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
getStreamingDistribution :: Text -- ^ 'gsdId'
                         -> GetStreamingDistribution
getStreamingDistribution p1 = GetStreamingDistribution
    { _gsdId = p1
    }

-- | The streaming distribution's id.
gsdId :: Lens' GetStreamingDistribution Text
gsdId = lens _gsdId (\s a -> s { _gsdId = a })

instance ToPath GetStreamingDistribution

instance ToQuery GetStreamingDistribution

instance ToHeaders GetStreamingDistribution

instance ToXML GetStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetStreamingDistributionRequest"

-- | The returned result of the corresponding request.
data GetStreamingDistributionResponse = GetStreamingDistributionResponse
    { _gsdrStreamingDistribution :: Maybe StreamingDistribution
    , _gsdrETag :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStreamingDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamingDistribution ::@ @Maybe StreamingDistribution@
--
-- * @ETag ::@ @Maybe Text@
--
getStreamingDistributionResponse :: GetStreamingDistributionResponse
getStreamingDistributionResponse = GetStreamingDistributionResponse
    { _gsdrStreamingDistribution = Nothing
    , _gsdrETag = Nothing
    }

-- | The streaming distribution's information.
gsdrStreamingDistribution :: Lens' GetStreamingDistributionResponse (Maybe StreamingDistribution)
gsdrStreamingDistribution =
    lens _gsdrStreamingDistribution
         (\s a -> s { _gsdrStreamingDistribution = a })

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdrETag :: Lens' GetStreamingDistributionResponse (Maybe Text)
gsdrETag = lens _gsdrETag (\s a -> s { _gsdrETag = a })

instance AWSRequest GetStreamingDistribution where
    type Sv GetStreamingDistribution = CloudFront
    type Rs GetStreamingDistribution = GetStreamingDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionResponse
            <*> xml %|? "StreamingDistribution"
            <*> hs ~:? "ETag"
