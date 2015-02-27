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

-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
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

-- | Delete an origin access identity.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteCloudFrontOriginAccessIdentity.html>
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
    (
    -- * Request
      DeleteCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , deleteCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , dcfoaiId
    , dcfoaiIfMatch

    -- * Response
    , DeleteCloudFrontOriginAccessIdentityResponse
    -- ** Response constructor
    , deleteCloudFrontOriginAccessIdentityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity
    { _dcfoaiId      :: Text
    , _dcfoaiIfMatch :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteCloudFrontOriginAccessIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcfoaiId' @::@ 'Text'
--
-- * 'dcfoaiIfMatch' @::@ 'Maybe' 'Text'
--
deleteCloudFrontOriginAccessIdentity :: Text -- ^ 'dcfoaiId'
                                     -> DeleteCloudFrontOriginAccessIdentity
deleteCloudFrontOriginAccessIdentity p1 = DeleteCloudFrontOriginAccessIdentity
    { _dcfoaiId      = p1
    , _dcfoaiIfMatch = Nothing
    }

-- | The origin access identity's id.
dcfoaiId :: Lens' DeleteCloudFrontOriginAccessIdentity Text
dcfoaiId = lens _dcfoaiId (\s a -> s { _dcfoaiId = a })

-- | The value of the ETag header you received from a previous GET or PUT request.
-- For example: E2QWRUHAPOMQZL.
dcfoaiIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity (Maybe Text)
dcfoaiIfMatch = lens _dcfoaiIfMatch (\s a -> s { _dcfoaiIfMatch = a })

data DeleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteCloudFrontOriginAccessIdentityResponse' constructor.
deleteCloudFrontOriginAccessIdentityResponse :: DeleteCloudFrontOriginAccessIdentityResponse
deleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse

instance ToPath DeleteCloudFrontOriginAccessIdentity where
    toPath DeleteCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-11-06/origin-access-identity/cloudfront/"
        , toText _dcfoaiId
        ]

instance ToQuery DeleteCloudFrontOriginAccessIdentity where
    toQuery = const mempty

instance ToHeaders DeleteCloudFrontOriginAccessIdentity where
    toHeaders DeleteCloudFrontOriginAccessIdentity{..} = mconcat
        [ "If-Match" =: _dcfoaiIfMatch
        ]

instance ToXMLRoot DeleteCloudFrontOriginAccessIdentity where
    toXMLRoot = const (namespaced ns "DeleteCloudFrontOriginAccessIdentity" [])

instance ToXML DeleteCloudFrontOriginAccessIdentity

instance AWSRequest DeleteCloudFrontOriginAccessIdentity where
    type Sv DeleteCloudFrontOriginAccessIdentity = CloudFront
    type Rs DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentityResponse

    request  = delete
    response = nullResponse DeleteCloudFrontOriginAccessIdentityResponse
