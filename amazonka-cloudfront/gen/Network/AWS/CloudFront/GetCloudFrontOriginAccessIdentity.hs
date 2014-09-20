{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about an origin access identity.
module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
    (
    -- * Request
      GetCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , getCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , gcfoaiId

    -- * Response
    , GetCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , getCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , gcfoairCloudFrontOriginAccessIdentity
    , gcfoairETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get an origin access identity's information.
newtype GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCloudFrontOriginAccessIdentity' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
getCloudFrontOriginAccessIdentity :: Text -- ^ 'gcfoaiId'
                                  -> GetCloudFrontOriginAccessIdentity
getCloudFrontOriginAccessIdentity p1 = GetCloudFrontOriginAccessIdentity
    { _gcfoaiId = p1
    }

-- | The identity's id.
gcfoaiId :: Lens' GetCloudFrontOriginAccessIdentity Text
gcfoaiId = lens _gcfoaiId (\s a -> s { _gcfoaiId = a })

instance ToPath GetCloudFrontOriginAccessIdentity

instance ToQuery GetCloudFrontOriginAccessIdentity

instance ToHeaders GetCloudFrontOriginAccessIdentity

instance ToXML GetCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetCloudFrontOriginAccessIdentityRequest"

-- | The returned result of the corresponding request.
data GetCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse
    { _gcfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _gcfoairETag :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCloudFrontOriginAccessIdentityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CloudFrontOriginAccessIdentity ::@ @Maybe CloudFrontOriginAccessIdentity@
--
-- * @ETag ::@ @Maybe Text@
--
getCloudFrontOriginAccessIdentityResponse :: GetCloudFrontOriginAccessIdentityResponse
getCloudFrontOriginAccessIdentityResponse = GetCloudFrontOriginAccessIdentityResponse
    { _gcfoairCloudFrontOriginAccessIdentity = Nothing
    , _gcfoairETag = Nothing
    }

-- | The origin access identity's information.
gcfoairCloudFrontOriginAccessIdentity :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
gcfoairCloudFrontOriginAccessIdentity =
    lens _gcfoairCloudFrontOriginAccessIdentity
         (\s a -> s { _gcfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the origin access identity's information. For
-- example: E2QWRUHAPOMQZL.
gcfoairETag :: Lens' GetCloudFrontOriginAccessIdentityResponse (Maybe Text)
gcfoairETag = lens _gcfoairETag (\s a -> s { _gcfoairETag = a })

instance AWSRequest GetCloudFrontOriginAccessIdentity where
    type Sv GetCloudFrontOriginAccessIdentity = CloudFront
    type Rs GetCloudFrontOriginAccessIdentity = GetCloudFrontOriginAccessIdentityResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "ETag"
