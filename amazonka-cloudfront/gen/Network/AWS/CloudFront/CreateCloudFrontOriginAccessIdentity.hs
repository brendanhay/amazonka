{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new origin access identity.
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
    (
    -- * Request
      CreateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , createCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ccfoaiCloudFrontOriginAccessIdentityConfig

    -- * Response
    , CreateCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , createCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ccfoairCloudFrontOriginAccessIdentity
    , ccfoairLocation
    , ccfoairETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to create a new origin access identity.
newtype CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCloudFrontOriginAccessIdentity' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CloudFrontOriginAccessIdentityConfig ::@ @CloudFrontOriginAccessIdentityConfig@
--
createCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ccfoaiCloudFrontOriginAccessIdentityConfig'
                                     -> CreateCloudFrontOriginAccessIdentity
createCloudFrontOriginAccessIdentity p1 = CreateCloudFrontOriginAccessIdentity
    { _ccfoaiCloudFrontOriginAccessIdentityConfig = p1
    }

-- | The origin access identity's configuration information.
ccfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CreateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ccfoaiCloudFrontOriginAccessIdentityConfig =
    lens _ccfoaiCloudFrontOriginAccessIdentityConfig
         (\s a -> s { _ccfoaiCloudFrontOriginAccessIdentityConfig = a })

instance ToPath CreateCloudFrontOriginAccessIdentity

instance ToQuery CreateCloudFrontOriginAccessIdentity

instance ToHeaders CreateCloudFrontOriginAccessIdentity

instance ToXML CreateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateCloudFrontOriginAccessIdentity"

-- | The returned result of the corresponding request.
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ccfoairLocation :: Maybe Text
    , _ccfoairETag :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCloudFrontOriginAccessIdentityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CloudFrontOriginAccessIdentity ::@ @Maybe CloudFrontOriginAccessIdentity@
--
-- * @Location ::@ @Maybe Text@
--
-- * @ETag ::@ @Maybe Text@
--
createCloudFrontOriginAccessIdentityResponse :: CreateCloudFrontOriginAccessIdentityResponse
createCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse
    { _ccfoairCloudFrontOriginAccessIdentity = Nothing
    , _ccfoairLocation = Nothing
    , _ccfoairETag = Nothing
    }

-- | The origin access identity's information.
ccfoairCloudFrontOriginAccessIdentity :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ccfoairCloudFrontOriginAccessIdentity =
    lens _ccfoairCloudFrontOriginAccessIdentity
         (\s a -> s { _ccfoairCloudFrontOriginAccessIdentity = a })

-- | The fully qualified URI of the new origin access identity just created. For
-- example:
-- https://cloudfront.amazonaws.com/2010-11-01/origin-access-identity/cloudfront/E74FTE3AJFJ256A.
-- 
ccfoairLocation :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairLocation = lens _ccfoairLocation (\s a -> s { _ccfoairLocation = a })

-- | The current version of the origin access identity created.
ccfoairETag :: Lens' CreateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ccfoairETag = lens _ccfoairETag (\s a -> s { _ccfoairETag = a })

instance AWSRequest CreateCloudFrontOriginAccessIdentity where
    type Sv CreateCloudFrontOriginAccessIdentity = CloudFront
    type Rs CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentityResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure CreateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "Location"
            <*> hs ~:? "ETag"
