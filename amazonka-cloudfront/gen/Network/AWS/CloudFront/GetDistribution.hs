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

-- Module      : Network.AWS.CloudFront.GetDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Get the information about a distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetDistribution.html>
module Network.AWS.CloudFront.GetDistribution
    (
    -- * Request
      GetDistribution
    -- ** Request constructor
    , getDistribution
    -- ** Request lenses
    , gdId

    -- * Response
    , GetDistributionResponse
    -- ** Response constructor
    , getDistributionResponse
    -- ** Response lenses
    , gdrDistribution
    , gdrETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetDistribution = GetDistribution
    { _gdId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdId' @::@ 'Text'
--
getDistribution :: Text -- ^ 'gdId'
                -> GetDistribution
getDistribution p1 = GetDistribution
    { _gdId = p1
    }

-- | The distribution's id.
gdId :: Lens' GetDistribution Text
gdId = lens _gdId (\s a -> s { _gdId = a })

data GetDistributionResponse = GetDistributionResponse
    { _gdrDistribution :: Maybe Distribution
    , _gdrETag         :: Maybe Text
    } deriving (Eq, Show)

-- | 'GetDistributionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrDistribution' @::@ 'Maybe' 'Distribution'
--
-- * 'gdrETag' @::@ 'Maybe' 'Text'
--
getDistributionResponse :: GetDistributionResponse
getDistributionResponse = GetDistributionResponse
    { _gdrDistribution = Nothing
    , _gdrETag         = Nothing
    }

-- | The distribution's information.
gdrDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrDistribution = lens _gdrDistribution (\s a -> s { _gdrDistribution = a })

-- | The current version of the distribution's information. For example:
-- E2QWRUHAPOMQZL.
gdrETag :: Lens' GetDistributionResponse (Maybe Text)
gdrETag = lens _gdrETag (\s a -> s { _gdrETag = a })

instance ToPath GetDistribution where
    toPath GetDistribution{..} = mconcat
        [ "/2014-10-21/distribution/"
        , toText _gdId
        ]

instance ToQuery GetDistribution where
    toQuery = const mempty

instance ToHeaders GetDistribution

instance ToXMLRoot GetDistribution where
    toXMLRoot = const (namespaced ns "GetDistribution" [])

instance ToXML GetDistribution

instance AWSRequest GetDistribution where
    type Sv GetDistribution = CloudFront
    type Rs GetDistribution = GetDistributionResponse

    request  = get
    response = xmlHeaderResponse $ \h x -> GetDistributionResponse
        <$> x .@? "Distribution"
        <*> h ~:? "ETag"
