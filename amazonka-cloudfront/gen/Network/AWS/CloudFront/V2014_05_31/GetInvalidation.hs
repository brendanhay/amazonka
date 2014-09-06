{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an invalidation.
module Network.AWS.CloudFront.V2014_05_31.GetInvalidation
    (
    -- * Request
      GetInvalidation
    -- ** Request constructor
    , mkGetInvalidation
    -- ** Request lenses
    , giDistributionId
    , giId

    -- * Response
    , GetInvalidationResponse
    -- ** Response lenses
    , girsInvalidation
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get an invalidation's information.
data GetInvalidation = GetInvalidation
    { _giDistributionId :: Text
    , _giId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetInvalidation' request.
mkGetInvalidation :: Text -- ^ 'giDistributionId'
                  -> Text -- ^ 'giId'
                  -> GetInvalidation
mkGetInvalidation p1 p2 = GetInvalidation
    { _giDistributionId = p1
    , _giId = p2
    }
{-# INLINE mkGetInvalidation #-}

-- | The distribution's id.
giDistributionId :: Lens' GetInvalidation Text
giDistributionId =
    lens _giDistributionId (\s a -> s { _giDistributionId = a })
{-# INLINE giDistributionId #-}

-- | The invalidation's id.
giId :: Lens' GetInvalidation Text
giId = lens _giId (\s a -> s { _giId = a })
{-# INLINE giId #-}

instance ToPath GetInvalidation where
    toPath GetInvalidation{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _giDistributionId
        , "/invalidation/"
        , toBS _giId
        ]

instance ToQuery GetInvalidation

instance ToHeaders GetInvalidation

instance ToXML GetInvalidation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetInvalidationRequest"

-- | The returned result of the corresponding request.
newtype GetInvalidationResponse = GetInvalidationResponse
    { _girsInvalidation :: Maybe Invalidation
    } deriving (Show, Generic)

-- | The invalidation's information.
girsInvalidation :: Lens' GetInvalidationResponse (Maybe Invalidation)
girsInvalidation =
    lens _girsInvalidation (\s a -> s { _girsInvalidation = a })
{-# INLINE girsInvalidation #-}

instance FromXML GetInvalidationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetInvalidation where
    type Sv GetInvalidation = CloudFront
    type Rs GetInvalidation = GetInvalidationResponse

    request = get
    response _ = xmlResponse
