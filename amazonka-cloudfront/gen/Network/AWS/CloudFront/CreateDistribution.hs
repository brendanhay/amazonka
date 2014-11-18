{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateDistribution.html>
module Network.AWS.CloudFront.CreateDistribution
    (
    -- * Request
      CreateDistribution
    -- ** Request constructor
    , createDistribution
    -- ** Request lenses
    , cdDistributionConfig

    -- * Response
    , CreateDistributionResponse
    -- ** Response constructor
    , createDistributionResponse
    -- ** Response lenses
    , cdrDistribution
    , cdrETag
    , cdrLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype CreateDistribution = CreateDistribution
    { _cdDistributionConfig :: DistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'CreateDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDistributionConfig' @::@ 'DistributionConfig'
--
createDistribution :: DistributionConfig -- ^ 'cdDistributionConfig'
                   -> CreateDistribution
createDistribution p1 = CreateDistribution
    { _cdDistributionConfig = p1
    }

-- | The distribution's configuration information.
cdDistributionConfig :: Lens' CreateDistribution DistributionConfig
cdDistributionConfig =
    lens _cdDistributionConfig (\s a -> s { _cdDistributionConfig = a })

data CreateDistributionResponse = CreateDistributionResponse
    { _cdrDistribution :: Maybe Distribution
    , _cdrETag         :: Maybe Text
    , _cdrLocation     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateDistributionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'cdrETag' @::@ 'Maybe' 'Text'
--
-- * 'cdrLocation' @::@ 'Maybe' 'Text'
--
createDistributionResponse :: CreateDistributionResponse
createDistributionResponse = CreateDistributionResponse
    { _cdrDistribution = Nothing
    , _cdrLocation     = Nothing
    , _cdrETag         = Nothing
    }

-- | The distribution's information.
cdrDistribution :: Lens' CreateDistributionResponse (Maybe Distribution)
cdrDistribution = lens _cdrDistribution (\s a -> s { _cdrDistribution = a })

-- | The current version of the distribution created.
cdrETag :: Lens' CreateDistributionResponse (Maybe Text)
cdrETag = lens _cdrETag (\s a -> s { _cdrETag = a })

-- | The fully qualified URI of the new distribution resource just created.
-- For example:
-- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
cdrLocation :: Lens' CreateDistributionResponse (Maybe Text)
cdrLocation = lens _cdrLocation (\s a -> s { _cdrLocation = a })

instance ToPath CreateDistribution where
    toPath = const "/2014-05-31/distribution"

instance ToQuery CreateDistribution where
    toQuery = const mempty

instance ToHeaders CreateDistribution

instance ToXML CreateDistribution where
    toXML CreateDistribution{..} = node "CreateDistribution"
        [ "DistributionConfig" .= _cdDistributionConfig
        ]

instance AWSRequest CreateDistribution where
    type Sv CreateDistribution = CloudFront
    type Rs CreateDistribution = CreateDistributionResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateDistributionResponse
        <$> x %| "Distribution"
        <*> h ~:? "ETag"
        <*> h ~:? "Location"
