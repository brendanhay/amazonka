{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , UpdateCloudFrontOriginAccessIdentityResult
    -- ** Response constructor
    , updateCloudFrontOriginAccessIdentityResult
    -- ** Response lenses
    , ucfoairCloudFrontOriginAccessIdentity
    , ucfoairETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentity
    { _ucfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    , _ucfoaiId                                   :: Text
    , _ucfoaiIfMatch                              :: Maybe Text
    } deriving (Eq, Show, Generic)

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

instance ToPath UpdateCloudFrontOriginAccessIdentity where
    toPath UpdateCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _ucfoaiId
        , "/config"
        ]

instance ToQuery UpdateCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders UpdateCloudFrontOriginAccessIdentity where
    toHeaders UpdateCloudFrontOriginAccessIdentity{..} = mconcat
        [ "If-Match" =: _ucfoaiIfMatch
        ]

instance ToBody UpdateCloudFrontOriginAccessIdentity where
    toBody = toBody . encodeXML . _ucfoaiCloudFrontOriginAccessIdentityConfig

data UpdateCloudFrontOriginAccessIdentityResult = UpdateCloudFrontOriginAccessIdentityResult
    { _ucfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ucfoairETag                           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateCloudFrontOriginAccessIdentityResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'ucfoairETag' @::@ 'Maybe' 'Text'
--
updateCloudFrontOriginAccessIdentityResult :: UpdateCloudFrontOriginAccessIdentityResult
updateCloudFrontOriginAccessIdentityResult = UpdateCloudFrontOriginAccessIdentityResult
    { _ucfoairCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairETag                           = Nothing
    }

-- | The origin access identity's information.
ucfoairCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentityResult (Maybe CloudFrontOriginAccessIdentity)
ucfoairCloudFrontOriginAccessIdentity =
    lens _ucfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _ucfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairETag :: Lens' UpdateCloudFrontOriginAccessIdentityResult (Maybe Text)
ucfoairETag = lens _ucfoairETag (\s a -> s { _ucfoairETag = a })

instance AWSRequest UpdateCloudFrontOriginAccessIdentity where
    type Sv UpdateCloudFrontOriginAccessIdentity = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity = UpdateCloudFrontOriginAccessIdentityResult

    request  = put
    response = const . xmlResponse $ \h x -> UpdateCloudFrontOriginAccessIdentityResult
record
