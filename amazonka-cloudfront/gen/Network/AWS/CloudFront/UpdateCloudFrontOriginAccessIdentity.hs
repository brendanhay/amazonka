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

-- Module      : Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
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

-- | Update an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateCloudFrontOriginAccessIdentity.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity
    { _ucfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    , _ucfoaiId                                   :: Text
    , _ucfoaiIfMatch                              :: Maybe Text
    } deriving (Eq, Show)

-- | 'UpdateCloudFrontOriginAccessIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoaiCloudFrontOriginAccessIdentityConfig' @::@ 'CloudFrontOriginAccessIdentityConfig'
--
-- * 'ucfoaiId' @::@ 'Text'
--
-- * 'ucfoaiIfMatch' @::@ 'Maybe' 'Text'
--
updateCloudFrontOriginAccessIdentity :: CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoaiCloudFrontOriginAccessIdentityConfig'
                                     -> Text -- ^ 'ucfoaiId'
                                     -> UpdateCloudFrontOriginAccessIdentity
updateCloudFrontOriginAccessIdentity p1 p2 = UpdateCloudFrontOriginAccessIdentity
    { _ucfoaiCloudFrontOriginAccessIdentityConfig = p1
    , _ucfoaiId                                   = p2
    , _ucfoaiIfMatch                              = Nothing
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

data UpdateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { _ucfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ucfoairETag                           :: Maybe Text
    } deriving (Eq, Show)

-- | 'UpdateCloudFrontOriginAccessIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'ucfoairETag' @::@ 'Maybe' 'Text'
--
updateCloudFrontOriginAccessIdentityResponse :: UpdateCloudFrontOriginAccessIdentityResponse
updateCloudFrontOriginAccessIdentityResponse = UpdateCloudFrontOriginAccessIdentityResponse
    { _ucfoairCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairETag                           = Nothing
    }

-- | The origin access identity's information.
ucfoairCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe CloudFrontOriginAccessIdentity)
ucfoairCloudFrontOriginAccessIdentity =
    lens _ucfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _ucfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairETag :: Lens' UpdateCloudFrontOriginAccessIdentityResponse (Maybe Text)
ucfoairETag = lens _ucfoairETag (\s a -> s { _ucfoairETag = a })

instance ToPath UpdateCloudFrontOriginAccessIdentity where
    toPath UpdateCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-10-21/origin-access-identity/cloudfront/"
        , toText _ucfoaiId
        , "/config"
        ]

instance ToQuery UpdateCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders UpdateCloudFrontOriginAccessIdentity where
    toHeaders UpdateCloudFrontOriginAccessIdentity{..} = mconcat
        [ "If-Match" =: _ucfoaiIfMatch
        ]

instance ToXMLRoot UpdateCloudFrontOriginAccessIdentity where
    toXMLRoot UpdateCloudFrontOriginAccessIdentity{..} = namespaced ns "UpdateCloudFrontOriginAccessIdentity"
        [ "CloudFrontOriginAccessIdentityConfig" =@ _ucfoaiCloudFrontOriginAccessIdentityConfig
        ]

instance ToXML UpdateCloudFrontOriginAccessIdentity

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Sv UpdateCloudFrontOriginAccessIdentity = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResponse

    request  = put
    response = xmlHeaderResponse $ \h x -> UpdateCloudFrontOriginAccessIdentityResponse
        <$> x .@? "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"
