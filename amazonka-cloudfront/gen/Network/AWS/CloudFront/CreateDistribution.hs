{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      CreateDistribution2014_05_31
    -- ** Request constructor
    , createDistribution2014_05_31
    -- ** Request lenses
    , cdDistributionConfig

    -- * Response
    , CreateDistribution2014_05_31Response
    -- ** Response constructor
    , createDistribution2014_05_31Response
    -- ** Response lenses
    , cdrDistribution
    , cdrETag
    , cdrLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype CreateDistribution2014_05_31 = CreateDistribution2014_05_31
    { _cdDistributionConfig :: DistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'CreateDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdDistributionConfig' @::@ 'DistributionConfig'
--
createDistribution2014_05_31 :: DistributionConfig -- ^ 'cdDistributionConfig'
                             -> CreateDistribution2014_05_31
createDistribution2014_05_31 p1 = CreateDistribution2014_05_31
    { _cdDistributionConfig = p1
    }

-- | The distribution's configuration information.
cdDistributionConfig :: Lens' CreateDistribution2014_05_31 DistributionConfig
cdDistributionConfig =
    lens _cdDistributionConfig (\s a -> s { _cdDistributionConfig = a })

instance ToPath CreateDistribution2014_05_31 where
    toPath = const "/2014-05-31/distribution"

instance ToQuery CreateDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders CreateDistribution2014_05_31

instance ToBody CreateDistribution2014_05_31 where
    toBody = toBody . encodeXML . _cdDistributionConfig

data CreateDistribution2014_05_31Response = CreateDistribution2014_05_31Response
    { _cdrDistribution :: Maybe Distribution
    , _cdrETag         :: Maybe Text
    , _cdrLocation     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateDistribution2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'cdrETag' @::@ 'Maybe' 'Text'
--
-- * 'cdrLocation' @::@ 'Maybe' 'Text'
--
createDistribution2014_05_31Response :: CreateDistribution2014_05_31Response
createDistribution2014_05_31Response = CreateDistribution2014_05_31Response
    { _cdrDistribution = Nothing
    , _cdrLocation     = Nothing
    , _cdrETag         = Nothing
    }

-- | The distribution's information.
cdrDistribution :: Lens' CreateDistribution2014_05_31Response (Maybe Distribution)
cdrDistribution = lens _cdrDistribution (\s a -> s { _cdrDistribution = a })

-- | The current version of the distribution created.
cdrETag :: Lens' CreateDistribution2014_05_31Response (Maybe Text)
cdrETag = lens _cdrETag (\s a -> s { _cdrETag = a })

-- | The fully qualified URI of the new distribution resource just created.
-- For example:
-- https://cloudfront.amazonaws.com/2010-11-01/distribution/EDFDVBD632BHDS5.
cdrLocation :: Lens' CreateDistribution2014_05_31Response (Maybe Text)
cdrLocation = lens _cdrLocation (\s a -> s { _cdrLocation = a })

instance AWSRequest CreateDistribution2014_05_31 where
    type Sv CreateDistribution2014_05_31 = CloudFront
    type Rs CreateDistribution2014_05_31 = CreateDistribution2014_05_31Response

    request  = post
    response = xmlResponse $ \h x -> CreateDistribution2014_05_31Response
        <$> x %| "Distribution"
        <*> h ~:? "ETag"
        <*> h ~:? "Location"
