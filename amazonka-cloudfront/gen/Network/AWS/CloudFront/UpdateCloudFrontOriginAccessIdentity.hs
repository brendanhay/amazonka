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
      UpdateCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request constructor
    , updateCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request lenses
    , ucfoaiCloudFrontOriginAccessIdentityConfig
    , ucfoaiId
    , ucfoaiIfMatch

    -- * Response
    , UpdateCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response constructor
    , updateCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response lenses
    , ucfoairCloudFrontOriginAccessIdentity
    , ucfoairETag
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data UpdateCloudFrontOriginAccessIdentity2014_05_31 = UpdateCloudFrontOriginAccessIdentity2014_05_31
    { _ucfoaiCloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
    , _ucfoaiId                                   :: Text
    , _ucfoaiIfMatch                              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateCloudFrontOriginAccessIdentity2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoaiCloudFrontOriginAccessIdentityConfig' @::@ 'CloudFrontOriginAccessIdentityConfig'
--
-- * 'ucfoaiId' @::@ 'Text'
--
-- * 'ucfoaiIfMatch' @::@ 'Maybe' 'Text'
--
updateCloudFrontOriginAccessIdentity2014_05_31 :: CloudFrontOriginAccessIdentityConfig -- ^ 'ucfoaiCloudFrontOriginAccessIdentityConfig'
                                               -> Text -- ^ 'ucfoaiId'
                                               -> UpdateCloudFrontOriginAccessIdentity2014_05_31
updateCloudFrontOriginAccessIdentity2014_05_31 p1 p2 = UpdateCloudFrontOriginAccessIdentity2014_05_31
    { _ucfoaiCloudFrontOriginAccessIdentityConfig = p1
    , _ucfoaiId                                   = p2
    , _ucfoaiIfMatch                              = Nothing
    }

-- | The identity's configuration information.
ucfoaiCloudFrontOriginAccessIdentityConfig :: Lens' UpdateCloudFrontOriginAccessIdentity2014_05_31 CloudFrontOriginAccessIdentityConfig
ucfoaiCloudFrontOriginAccessIdentityConfig =
    lens _ucfoaiCloudFrontOriginAccessIdentityConfig
        (\s a -> s { _ucfoaiCloudFrontOriginAccessIdentityConfig = a })

-- | The identity's id.
ucfoaiId :: Lens' UpdateCloudFrontOriginAccessIdentity2014_05_31 Text
ucfoaiId = lens _ucfoaiId (\s a -> s { _ucfoaiId = a })

-- | The value of the ETag header you received when retrieving the identity's
-- configuration. For example: E2QWRUHAPOMQZL.
ucfoaiIfMatch :: Lens' UpdateCloudFrontOriginAccessIdentity2014_05_31 (Maybe Text)
ucfoaiIfMatch = lens _ucfoaiIfMatch (\s a -> s { _ucfoaiIfMatch = a })

instance ToPath UpdateCloudFrontOriginAccessIdentity2014_05_31 where
    toPath UpdateCloudFrontOriginAccessIdentity2014_05_31{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _ucfoaiId
        , "/config"
        ]

instance ToQuery UpdateCloudFrontOriginAccessIdentity2014_05_31 where
    toQuery = const mempty

instance ToHeaders UpdateCloudFrontOriginAccessIdentity2014_05_31 where
    toHeaders UpdateCloudFrontOriginAccessIdentity2014_05_31{..} = mconcat
        [ "If-Match" =: _ucfoaiIfMatch
        ]

instance ToBody UpdateCloudFrontOriginAccessIdentity2014_05_31 where
    toBody = toBody . encodeXML . _ucfoaiCloudFrontOriginAccessIdentityConfig

data UpdateCloudFrontOriginAccessIdentity2014_05_31Response = UpdateCloudFrontOriginAccessIdentity2014_05_31Response
    { _ucfoairCloudFrontOriginAccessIdentity :: Maybe CloudFrontOriginAccessIdentity
    , _ucfoairETag                           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UpdateCloudFrontOriginAccessIdentity2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucfoairCloudFrontOriginAccessIdentity' @::@ 'Maybe' 'CloudFrontOriginAccessIdentity'
--
-- * 'ucfoairETag' @::@ 'Maybe' 'Text'
--
updateCloudFrontOriginAccessIdentity2014_05_31Response :: UpdateCloudFrontOriginAccessIdentity2014_05_31Response
updateCloudFrontOriginAccessIdentity2014_05_31Response = UpdateCloudFrontOriginAccessIdentity2014_05_31Response
    { _ucfoairCloudFrontOriginAccessIdentity = Nothing
    , _ucfoairETag                           = Nothing
    }

-- | The origin access identity's information.
ucfoairCloudFrontOriginAccessIdentity :: Lens' UpdateCloudFrontOriginAccessIdentity2014_05_31Response (Maybe CloudFrontOriginAccessIdentity)
ucfoairCloudFrontOriginAccessIdentity =
    lens _ucfoairCloudFrontOriginAccessIdentity
        (\s a -> s { _ucfoairCloudFrontOriginAccessIdentity = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
ucfoairETag :: Lens' UpdateCloudFrontOriginAccessIdentity2014_05_31Response (Maybe Text)
ucfoairETag = lens _ucfoairETag (\s a -> s { _ucfoairETag = a })

instance AWSRequest UpdateCloudFrontOriginAccessIdentity2014_05_31 where
    type Sv UpdateCloudFrontOriginAccessIdentity2014_05_31 = CloudFront
    type Rs UpdateCloudFrontOriginAccessIdentity2014_05_31 = UpdateCloudFrontOriginAccessIdentity2014_05_31Response

    request  = put
    response = xmlResponse $ \h x -> UpdateCloudFrontOriginAccessIdentity2014_05_31Response
        <$> x %| "CloudFrontOriginAccessIdentity"
        <*> h ~:? "ETag"
