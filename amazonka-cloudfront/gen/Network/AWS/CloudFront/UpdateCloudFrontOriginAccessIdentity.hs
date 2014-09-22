{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update an origin access identity.
module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
    (
    -- * Request
      UpdateCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , updateCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , ucfoaiCloudFrontOriginAccessIdentityConfig
    , ucfoaiId
    , ucfoaiIfMatch

    -- * Response
    , UpdateCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , updateCloudFrontOriginAccessIdentityResponse
    -- ** Response lenses
    , ucfoairCloudFrontOriginAccessIdentity
    , ucfoairETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to update an origin access identity.
data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity
    { _ucfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    , _ucfoaiId :: Text
    , _ucfoaiIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateCloudFrontOriginAccessIdentity' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CloudFrontOriginAccessIdentityConfig ::@ @CloudFrontOriginAccessIdentityConfig@
--
-- * @Id ::@ @Text@
--
-- * @IfMatch ::@ @Maybe Text@
--
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoaiCloudFrontOriginAccessIdentityConfig'
                                     -> Text -- ^ 'ucfoaiId'
                                     -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity p1 p2 = UpdateCloudFrontOriginAccessIdentity
    { _ucfoaiCloudFrontOriginAccessIdentityConfig = p1
    , _ucfoaiId = p2
    , _ucfoaiIfMatch = Nothing
    }

-- | The identity's configuration information.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig =
    lens _ucfoaiCloudFrontOriginAccessIdentityConfig
         (\s a -> s { _ucfoaiCloudFrontOriginAccessIdentityConfig = a })

-- | The identity's id.
ucfoaiId :: Lens' UpdateCloudFrontOriginAccessIdentity Text
ucfoaiId = lens _ucfoaiId (\s a -> s { _ucfoaiId = a })

-- | The value of the ETag header you received when retrieving the identity's
-- configuration. For example: E2QWRUHAPOMQZL.
ucfoaiIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity (Maybe Text)
ucfoaiIfMatch = lens _ucfoaiIfMatch (\s a -> s { _ucfoaiIfMatch = a })

instance ToPath UpdateCloudFrontOriginAccessIdentity

instance ToQuery UpdateCloudFrontOriginAccessIdentity

instance ToHeaders UpdateCloudFrontOriginAccessIdentity where
    toHeaders UpdateCloudFrontOriginAccessIdentity{..} = concat
        [ "If-Match" =: _ucfoaiIfMatch
        ]

instance ToXML UpdateCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateCloudFrontOriginAccessIdentity"

-- | The returned result of the corresponding request.
data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { _ucfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ucfoairETag :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateCloudFrontOriginAccessIdentityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CloudFrontOriginAccessIdentity ::@ @Maybe CloudFrontOriginAccessIdentity@
--
-- * @ETag ::@ @Maybe Text@
--
updateCloudFrontOriginAccessIdentityResponse :: UpdateCloudFrontOriginAccessIdentityResponse
updateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { _ucfoairCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairETag = Nothing
    }

-- | The origin access identity's information.
ucfoairCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ucfoairCloudFrontOriginAccessIdentity =
    lens _ucfoairCloudFrontOriginAccessIdentity
         (\s a -> s { _ucfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairETag :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ucfoairETag = lens _ucfoairETag (\s a -> s { _ucfoairETag = a })

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Sv UpdateCloudFrontOriginAccessIdentity = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure UpdateCloudFrontOriginAccessIdentityResponse
            <*> xml %|? "CloudFrontOriginAccessIdentity"
            <*> hs ~:? "ETag"
