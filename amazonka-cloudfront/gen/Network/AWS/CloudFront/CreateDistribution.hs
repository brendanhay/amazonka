{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new distribution.
module Network.AWS.CloudFront.CreateDistribution
    (
    -- * Request
      CreateDistribution
    -- ** Request constructor
    , mkCreateDistribution
    -- ** Request lenses
    , cdDistributionConfig

    -- * Response
    , CreateDistributionResponse
    -- ** Response constructor
    , mkCreateDistributionResponse
    -- ** Response lenses
    , cdrDistribution
    , cdrLocation
    , cdrETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create a new distribution.
newtype CreateDistribution = CreateDistribution
    { _cdDistributionConfig :: DistributionConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DistributionConfig ::@ @DistributionConfig@
--
mkCreateDistribution :: DistributionConfig -- ^ 'cdDistributionConfig'
                     -> CreateDistribution
mkCreateDistribution p1 = CreateDistribution
    { _cdDistributionConfig = p1
    }

-- | The distribution's configuration information.
cdDistributionConfig :: Lens' CreateDistribution DistributionConfig
cdDistributionConfig =
    lens _cdDistributionConfig (\s a -> s { _cdDistributionConfig = a })

instance ToPath CreateDistribution

instance ToQuery CreateDistribution

instance ToHeaders CreateDistribution

instance ToXML CreateDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateDistributionRequest"

-- | The returned result of the corresponding request.
data CreateDistributionResponse = CreateDistributionResponse
    { _cdrDistribution :: Maybe Distribution
    , _cdrLocation :: Maybe Text
    , _cdrETag :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Distribution ::@ @Maybe Distribution@
--
-- * @Location ::@ @Maybe Text@
--
-- * @ETag ::@ @Maybe Text@
--
mkCreateDistributionResponse :: CreateDistributionResponse
mkCreateDistributionResponse = CreateDistributionResponse
    { _cdrDistribution = Nothing
    , _cdrLocation = Nothing
    , _cdrETag = Nothing
    }

-- | The distribution's information.
cdrDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdrDistribution = lens _cdrDistribution (\s a -> s { _cdrDistribution = a })

-- | The fully qualified URI of the new distribution resource just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
cdrLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdrLocation = lens _cdrLocation (\s a -> s { _cdrLocation = a })

-- | The current version of the distribution created.
cdrETag :: Lens' CreateDistributionResponse (Maybe Text)
cdrETag = lens _cdrETag (\s a -> s { _cdrETag = a })

instance AWSRequest CreateDistribution where
    type Sv CreateDistribution = CloudFront
    type Rs CreateDistribution = CreateDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
