{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete an origin access identity.
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
    (
    -- * Request
      DeleteCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request constructor
    , deleteCloudFrontOriginAccessIdentity2014_05_31
    -- ** Request lenses
    , dcfoaiId
    , dcfoaiIfMatch

    -- * Response
    , DeleteCloudFrontOriginAccessIdentity2014_05_31Response
    -- ** Response constructor
    , deleteCloudFrontOriginAccessIdentity2014_05_31Response
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data DeleteCloudFrontOriginAccessIdentity2014_05_31 = DeleteCloudFrontOriginAccessIdentity2014_05_31
    { _dcfoaiId      :: Text
    , _dcfoaiIfMatch :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCloudFrontOriginAccessIdentity2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcfoaiId' @::@ 'Text'
--
-- * 'dcfoaiIfMatch' @::@ 'Maybe' 'Text'
--
deleteCloudFrontOriginAccessIdentity2014_05_31 :: Text -- ^ 'dcfoaiId'
                                               -> DeleteCloudFrontOriginAccessIdentity2014_05_31
deleteCloudFrontOriginAccessIdentity2014_05_31 p1 = DeleteCloudFrontOriginAccessIdentity2014_05_31
    { _dcfoaiId      = p1
    , _dcfoaiIfMatch = Nothing
    }

-- | The origin access identity's id.
dcfoaiId :: Lens' DeleteCloudFrontOriginAccessIdentity2014_05_31 Text
dcfoaiId = lens _dcfoaiId (\s a -> s { _dcfoaiId = a })

-- | The value of the ETag header you received from a previous GET or PUT
-- request. For example: E2QWRUHAPOMQZL.
dcfoaiIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity2014_05_31 (Maybe Text)
dcfoaiIfMatch = lens _dcfoaiIfMatch (\s a -> s { _dcfoaiIfMatch = a })

instance ToPath DeleteCloudFrontOriginAccessIdentity2014_05_31 where
    toPath DeleteCloudFrontOriginAccessIdentity2014_05_31{..} = mconcat
        [ "/2014-05-31/origin-access-identity/cloudfront/"
        , toText _dcfoaiId
        ]

instance ToQuery DeleteCloudFrontOriginAccessIdentity2014_05_31 where
    toQuery = const mempty

instance ToHeaders DeleteCloudFrontOriginAccessIdentity2014_05_31 where
    toHeaders DeleteCloudFrontOriginAccessIdentity2014_05_31{..} = mconcat
        [ "If-Match" =: _dcfoaiIfMatch
        ]

data DeleteCloudFrontOriginAccessIdentity2014_05_31Response = DeleteCloudFrontOriginAccessIdentity2014_05_31Response
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCloudFrontOriginAccessIdentity2014_05_31Response' constructor.
deleteCloudFrontOriginAccessIdentity2014_05_31Response :: DeleteCloudFrontOriginAccessIdentity2014_05_31Response
deleteCloudFrontOriginAccessIdentity2014_05_31Response = DeleteCloudFrontOriginAccessIdentity2014_05_31Response

instance AWSRequest DeleteCloudFrontOriginAccessIdentity2014_05_31 where
    type Sv DeleteCloudFrontOriginAccessIdentity2014_05_31 = CloudFront
    type Rs DeleteCloudFrontOriginAccessIdentity2014_05_31 = DeleteCloudFrontOriginAccessIdentity2014_05_31Response

    request  = delete
    response = nullaryResponse DeleteCloudFrontOriginAccessIdentity2014_05_31Response
