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
    , createDistribution
    -- ** Request lenses
    , cdrDistributionConfig

    -- * Response
    , CreateDistributionResponse
    -- ** Response lenses
    , cdsDistribution
    , cdsLocation
    , cdsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateDistribution' request.
createDistribution :: DistributionConfig -- ^ 'cdrDistributionConfig'
                   -> CreateDistribution
createDistribution p1 = CreateDistribution
    { _cdrDistributionConfig = p1
    }
{-# INLINE createDistribution #-}

data CreateDistribution = CreateDistribution
    { _cdrDistributionConfig :: DistributionConfig
      -- ^ The distribution's configuration information.
    } deriving (Show, Generic)

-- | The distribution's configuration information.
cdrDistributionConfig :: Lens' CreateDistribution (DistributionConfig)
cdrDistributionConfig f x =
    f (_cdrDistributionConfig x)
        <&> \y -> x { _cdrDistributionConfig = y }
{-# INLINE cdrDistributionConfig #-}

instance ToPath CreateDistribution where
    toPath = const "/2014-05-31/distribution"

instance ToQuery CreateDistribution

instance ToHeaders CreateDistribution

instance ToXML CreateDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateDistributionRequest"

data CreateDistributionResponse = CreateDistributionResponse
    { _cdsDistribution :: Maybe Distribution
      -- ^ The distribution's information.
    , _cdsLocation :: Maybe Text
      -- ^ The fully qualified URI of the new distribution resource just
      -- created. For example:
      -- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
      -- 
    , _cdsETag :: Maybe Text
      -- ^ The current version of the distribution created.
    } deriving (Show, Generic)

-- | The distribution's information.
cdsDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdsDistribution f x =
    f (_cdsDistribution x)
        <&> \y -> x { _cdsDistribution = y }
{-# INLINE cdsDistribution #-}

-- | The fully qualified URI of the new distribution resource just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
cdsLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdsLocation f x =
    f (_cdsLocation x)
        <&> \y -> x { _cdsLocation = y }
{-# INLINE cdsLocation #-}

-- | The current version of the distribution created.
cdsETag :: Lens' CreateDistributionResponse (Maybe Text)
cdsETag f x =
    f (_cdsETag x)
        <&> \y -> x { _cdsETag = y }
{-# INLINE cdsETag #-}

instance AWSRequest CreateDistribution where
    type Sv CreateDistribution = CloudFront
    type Rs CreateDistribution = CreateDistributionResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CreateDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
