{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity2014_11_06
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/DeleteCloudFrontOriginAccessIdentity2014_11_06.html>
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity2014_11_06
    (
    -- * Request
      DeleteCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request constructor
    , deleteCloudFrontOriginAccessIdentity2014_11_06
    -- ** Request lenses
    , dcfoaiIfMatch
    , dcfoaiId

    -- * Response
    , DeleteCloudFrontOriginAccessIdentity2014_11_06Response
    -- ** Response constructor
    , deleteCloudFrontOriginAccessIdentity2014_11_06Response
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFront.Types

-- | /See:/ 'deleteCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcfoaiIfMatch'
--
-- * 'dcfoaiId'
data DeleteCloudFrontOriginAccessIdentity2014_11_06 = DeleteCloudFrontOriginAccessIdentity2014_11_06'{_dcfoaiIfMatch :: Maybe Text, _dcfoaiId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteCloudFrontOriginAccessIdentity2014_11_06' smart constructor.
deleteCloudFrontOriginAccessIdentity2014_11_06 :: Text -> DeleteCloudFrontOriginAccessIdentity2014_11_06
deleteCloudFrontOriginAccessIdentity2014_11_06 pId = DeleteCloudFrontOriginAccessIdentity2014_11_06'{_dcfoaiIfMatch = Nothing, _dcfoaiId = pId};

-- | The value of the ETag header you received from a previous GET or PUT
-- request. For example: E2QWRUHAPOMQZL.
dcfoaiIfMatch :: Lens' DeleteCloudFrontOriginAccessIdentity2014_11_06 (Maybe Text)
dcfoaiIfMatch = lens _dcfoaiIfMatch (\ s a -> s{_dcfoaiIfMatch = a});

-- | The origin access identity\'s id.
dcfoaiId :: Lens' DeleteCloudFrontOriginAccessIdentity2014_11_06 Text
dcfoaiId = lens _dcfoaiId (\ s a -> s{_dcfoaiId = a});

instance AWSRequest
         DeleteCloudFrontOriginAccessIdentity2014_11_06 where
        type Sv
               DeleteCloudFrontOriginAccessIdentity2014_11_06
             = CloudFront
        type Rs
               DeleteCloudFrontOriginAccessIdentity2014_11_06
             =
             DeleteCloudFrontOriginAccessIdentity2014_11_06Response
        request = delete
        response
          = receiveNull
              DeleteCloudFrontOriginAccessIdentity2014_11_06Response'

instance ToHeaders
         DeleteCloudFrontOriginAccessIdentity2014_11_06 where
        toHeaders
          DeleteCloudFrontOriginAccessIdentity2014_11_06'{..}
          = mconcat ["If-Match" =# _dcfoaiIfMatch]

instance ToPath
         DeleteCloudFrontOriginAccessIdentity2014_11_06 where
        toPath
          DeleteCloudFrontOriginAccessIdentity2014_11_06'{..}
          = mconcat
              ["/2014-11-06/origin-access-identity/cloudfront/",
               toText _dcfoaiId]

instance ToQuery
         DeleteCloudFrontOriginAccessIdentity2014_11_06 where
        toQuery = const mempty

-- | /See:/ 'deleteCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
data DeleteCloudFrontOriginAccessIdentity2014_11_06Response = DeleteCloudFrontOriginAccessIdentity2014_11_06Response' deriving (Eq, Read, Show)

-- | 'DeleteCloudFrontOriginAccessIdentity2014_11_06Response' smart constructor.
deleteCloudFrontOriginAccessIdentity2014_11_06Response :: DeleteCloudFrontOriginAccessIdentity2014_11_06Response
deleteCloudFrontOriginAccessIdentity2014_11_06Response = DeleteCloudFrontOriginAccessIdentity2014_11_06Response';
