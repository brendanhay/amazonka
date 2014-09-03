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
    , getDistribution
    -- ** Request lenses
    , gdrId

    -- * Response
    , GetDistributionResponse
    -- ** Response lenses
    , gdsDistribution
    , gdsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetDistribution' request.
getDistribution :: Text -- ^ 'gdrId'
                -> GetDistribution
getDistribution p1 = GetDistribution
    { _gdrId = p1
    }

data GetDistribution = GetDistribution
    { _gdrId :: Text
      -- ^ The distribution's id.
    } deriving (Show, Generic)

-- | The distribution's id.
gdrId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetDistribution
    -> f GetDistribution
gdrId f x =
    (\y -> x { _gdrId = y })
       <$> f (_gdrId x)
{-# INLINE gdrId #-}

instance ToPath GetDistribution where
    toPath GetDistribution{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _gdrId
        ]

instance ToQuery GetDistribution

instance ToHeaders GetDistribution

instance ToXML GetDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetDistributionRequest"

data GetDistributionResponse = GetDistributionResponse
    { _gdsDistribution :: Maybe Distribution
      -- ^ The distribution's information.
    , _gdsETag :: Maybe Text
      -- ^ The current version of the distribution's information. For
      -- example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The distribution's information.
gdsDistribution
    :: Functor f
    => (Maybe Distribution
    -> f (Maybe Distribution))
    -> GetDistributionResponse
    -> f GetDistributionResponse
gdsDistribution f x =
    (\y -> x { _gdsDistribution = y })
       <$> f (_gdsDistribution x)
{-# INLINE gdsDistribution #-}

-- | The current version of the distribution's information. For example:
-- E2QWRUHAPOMQZL.
gdsETag
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetDistributionResponse
    -> f GetDistributionResponse
gdsETag f x =
    (\y -> x { _gdsETag = y })
       <$> f (_gdsETag x)
{-# INLINE gdsETag #-}

instance AWSRequest GetDistribution where
    type Sv GetDistribution = CloudFront
    type Rs GetDistribution = GetDistributionResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetDistributionResponse
            <*> xml %|? "Distribution"
            <*> hs ~:? "ETag"
