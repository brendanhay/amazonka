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
    , mkDeleteCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , dcfoaiId
    , dcfoaiIfMatch

    -- * Response
    , DeleteCloudFrontOriginAccessIdentityResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to delete a origin access identity.
data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity
    { _dcfoaiId :: Text
    , _dcfoaiIfMatch :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteCloudFrontOriginAccessIdentity' request.
mkDeleteCloudFrontOriginAccessIdentity :: Text -- ^ 'dcfoaiId'
                                       -> DeleteCloudFrontOriginAccessIdentity
mkDeleteCloudFrontOriginAccessIdentity p1 = DeleteCloudFrontOriginAccessIdentity
    { _dcfoaiId = p1
    , _dcfoaiIfMatch = Nothing
    }

-- | The origin access identity's id.
dcfoaiId :: Lens' DeleteCloudFrontOriginAccessIdentity Text
dcfoaiId = lens _dcfoaiId (\s a -> s { _dcfoaiId = a })

-- | The value of the ETag header you received from a previous GET or PUT
-- request. For example: E2QWRUHAPOMQZL.
dcfoaiIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity (Maybe Text)
dcfoaiIfMatch = lens _dcfoaiIfMatch (\s a -> s { _dcfoaiIfMatch = a })

instance ToPath DeleteCloudFrontOriginAccessIdentity where
    toPath DeleteCloudFrontOriginAccessIdentity{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toBS _dcfoaiId
        ]

instance ToQuery DeleteCloudFrontOriginAccessIdentity

instance ToHeaders DeleteCloudFrontOriginAccessIdentity where
    toHeaders DeleteCloudFrontOriginAccessIdentity{..} = concat
        [ "If-Match" =: _dcfoaiIfMatch
        ]

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
