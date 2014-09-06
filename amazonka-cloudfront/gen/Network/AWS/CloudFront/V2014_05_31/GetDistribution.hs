{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudFront.V2014_05_31.GetDistribution
    (
    -- * Request
      GetDistribution
    -- ** Request constructor
    , mkGetDistribution
    -- ** Request lenses
    , gdId

    -- * Response
    , GetDistributionResponse
    -- ** Response lenses
    , gdrsDistribution
    , gdrsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get a distribution's information.
newtype GetDistribution = GetDistribution
    { _gdId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDistribution' request.
mkGetDistribution :: Text -- ^ 'gdId'
                  -> GetDistribution
mkGetDistribution p1 = GetDistribution
    { _gdId = p1
    }
{-# INLINE mkGetDistribution #-}

-- | The distribution's id.
gdId :: Lens' GetDistribution Text
gdId = lens _gdId (\s a -> s { _gdId = a })
{-# INLINE gdId #-}

instance ToPath GetDistribution where
    toPath GetDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _gdId
        ]

instance ToQuery GetDistribution

instance ToHeaders GetDistribution

instance ToXML GetDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionRequest"

-- | The returned result of the corresponding request.
data GetDistributionResponse = GetDistributionResponse
    { _gdrsDistribution :: Maybe Distribution
    , _gdrsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The distribution's information.
gdrsDistribution :: Lens' GetDistributionResponse (Maybe Distribution)
gdrsDistribution =
    lens _gdrsDistribution (\s a -> s { _gdrsDistribution = a })
{-# INLINE gdrsDistribution #-}

-- | The current version of the distribution's information. For example:
-- E2QWRUHAPOMQZL.
gdrsETag :: Lens' GetDistributionResponse (Maybe Text)
gdrsETag = lens _gdrsETag (\s a -> s { _gdrsETag = a })
{-# INLINE gdrsETag #-}

instance AWSRequest GetDistribution where
    type Sv GetDistribution = CloudFront
    type Rs GetDistribution = GetDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "ETag"
