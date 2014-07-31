{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a distribution.
module Network.AWS.CloudFront.V2014_05_31.GetDistribution where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

data GetDistribution = GetDistribution
    { _gdrId :: Text
      -- ^ The distribution's id.
    } deriving (Generic)

instance ToPath GetDistribution where
    toPath GetDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _gdrId
        ]

instance ToQuery GetDistribution

instance ToHeaders GetDistribution

instance ToXML GetDistributionRequest where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionRequest"

instance AWSRequest GetDistribution where
    type Sv GetDistribution = CloudFront
    type Rs GetDistribution = GetDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "ETag"

data GetDistributionResponse = GetDistributionResponse
    { _gdsDistribution :: Maybe Distribution
      -- ^ The distribution's information.
    , _gdsETag :: Maybe Text
      -- ^ The current version of the distribution's information. For
      -- example: E2QWRUHAPOMQZL.
    } deriving (Generic)
