{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Get the information about a distribution.
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

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get a distribution's information.
newtype GetDistribution = GetDistribution
    { _gdId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistribution' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
getDistribution :: Text -- ^ 'gdId'
                -> GetDistribution
getDistribution p1 = GetDistribution
    { _gdId = p1
    }

-- | The distribution's id.
gdId :: Lens' GetDistribution Text
gdId = lens _gdId (\s a -> s { _gdId = a })

instance ToPath GetDistribution

instance ToQuery GetDistribution

instance ToHeaders GetDistribution

instance ToXML GetDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionRequest"

-- | The returned result of the corresponding request.
data GetDistributionResponse = GetDistributionResponse
    { _gdrDistribution :: Maybe Distribution
    , _gdrETag :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistributionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Distribution ::@ @Maybe Distribution@
--
-- * @ETag ::@ @Maybe Text@
--
getDistributionResponse :: GetDistributionResponse
getDistributionResponse = GetDistributionResponse
    { _gdrDistribution = Nothing
    , _gdrETag = Nothing
    }

-- | The distribution's information.
gdrDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrDistribution = lens _gdrDistribution (\s a -> s { _gdrDistribution = a })

-- | The current version of the distribution's information. For example:
-- E2QWRUHAPOMQZL.
gdrETag :: Lens' GetDistributionResponse (Maybe Text)
gdrETag = lens _gdrETag (\s a -> s { _gdrETag = a })

instance AWSRequest GetDistribution where
    type Sv GetDistribution = CloudFront
    type Rs GetDistribution = GetDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "ETag"
