{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete an origin access identity.
module Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity
    (
    -- * Request
      DeleteCloudFrontOriginAccessIdentity
    -- ** Request constructor
    , mkDeleteCloudFrontOriginAccessIdentityRequest
    -- ** Request lenses
    , dcfoairId
    , dcfoairIfMatch

    -- * Response
    , DeleteCloudFrontOriginAccessIdentityResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCloudFrontOriginAccessIdentity' request.
mkDeleteCloudFrontOriginAccessIdentityRequest :: Text -- ^ 'dcfoairId'
                                              -> DeleteCloudFrontOriginAccessIdentity
mkDeleteCloudFrontOriginAccessIdentityRequest p1 = DeleteCloudFrontOriginAccessIdentity
    { _dcfoairId = p1
    , _dcfoairIfMatch = Nothing
    }
{-# INLINE mkDeleteCloudFrontOriginAccessIdentityRequest #-}

data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity
    { _dcfoairId :: Text
      -- ^ The origin access identity's id.
    , _dcfoairIfMatch :: Maybe Text
      -- ^ The value of the ETag header you received from a previous GET or
      -- PUT request. For example: E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The origin access identity's id.
dcfoairId :: Lens' DeleteCloudFrontOriginAccessIdentity (Text)
dcfoairId = lens _dcfoairId (\s a -> s { _dcfoairId = a })
{-# INLINE dcfoairId #-}

-- | The value of the ETag header you received from a previous GET or PUT
-- request. For example: E2QWRUHAPOMQZL.
dcfoairIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity (Maybe Text)
dcfoairIfMatch = lens _dcfoairIfMatch (\s a -> s { _dcfoairIfMatch = a })
{-# INLINE dcfoairIfMatch #-}

instance ToPath DeleteCloudFrontOriginAccessIdentity where
    toPath DeleteCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toBS _dcfoairId
        ]

instance ToQuery DeleteCloudFrontOriginAccessIdentity

instance ToHeaders DeleteCloudFrontOriginAccessIdentity

instance ToXML DeleteCloudFrontOriginAccessIdentity where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteCloudFrontOriginAccessIdentityRequest"

data DeleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteCloudFrontOriginAccessIdentity where
    type Sv DeleteCloudFrontOriginAccessIdentity = CloudFront
    type Rs DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentityResponse

    request = delete
    response _ = nullaryResponse DeleteCloudFrontOriginAccessIdentityResponse
