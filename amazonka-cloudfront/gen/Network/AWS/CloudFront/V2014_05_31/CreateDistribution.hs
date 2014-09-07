{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.CreateDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new distribution.
module Network.AWS.CloudFront.V2014_05_31.CreateDistribution
    (
    -- * Request
      CreateDistribution
    -- ** Request constructor
    , mkCreateDistribution
    -- ** Request lenses
    , cdDistributionConfig

    -- * Response
    , CreateDistributionResponse
    -- ** Response lenses
    , cdrsDistribution
    , cdrsLocation
    , cdrsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create a new distribution.
newtype CreateDistribution = CreateDistribution
    { _cdDistributionConfig :: DistributionConfig
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDistribution' request.
mkCreateDistribution :: DistributionConfig -- ^ 'cdDistributionConfig'
                     -> CreateDistribution
mkCreateDistribution p1 = CreateDistribution
    { _cdDistributionConfig = p1
    }

-- | The distribution's configuration information.
cdDistributionConfig :: Lens' CreateDistribution DistributionConfig
cdDistributionConfig =
    lens _cdDistributionConfig (\s a -> s { _cdDistributionConfig = a })

instance ToPath CreateDistribution where
    toPath = const "/2014-05-31/distribution"

instance ToQuery CreateDistribution

instance ToHeaders CreateDistribution

instance ToXML CreateDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateDistributionRequest"

-- | The returned result of the corresponding request.
data CreateDistributionResponse = CreateDistributionResponse
    { _cdrsDistribution :: Maybe Distribution
    , _cdrsLocation :: Maybe Text
    , _cdrsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The distribution's information.
cdrsDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdrsDistribution =
    lens _cdrsDistribution (\s a -> s { _cdrsDistribution = a })

-- | The fully qualified URI of the new distribution resource just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
cdrsLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdrsLocation = lens _cdrsLocation (\s a -> s { _cdrsLocation = a })

-- | The current version of the distribution created.
cdrsETag :: Lens' CreateDistributionResponse (Maybe Text)
cdrsETag = lens _cdrsETag (\s a -> s { _cdrsETag = a })

instance AWSRequest CreateDistribution where
    type Sv CreateDistribution = CloudFront
    type Rs CreateDistribution = CreateDistributionResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
