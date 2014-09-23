{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the region the bucket resides in.
module Network.AWS.S3.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , getBucketLocation
    -- ** Request lenses
    , gbl1Bucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response constructor
    , getBucketLocationResponse
    -- ** Response lenses
    , gblrrLocationConstraint
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketLocation = GetBucketLocation
    { _gbl1Bucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLocation' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
getBucketLocation :: BucketName -- ^ 'gbl1Bucket'
                  -> GetBucketLocation
getBucketLocation p1 = GetBucketLocation
    { _gbl1Bucket = p1
    }

gbl1Bucket :: Lens' GetBucketLocation BucketName
gbl1Bucket = lens _gbl1Bucket (\s a -> s { _gbl1Bucket = a })

instance ToPath GetBucketLocation

instance ToQuery GetBucketLocation

instance ToHeaders GetBucketLocation

instance ToBody GetBucketLocation

newtype GetBucketLocationResponse = GetBucketLocationResponse
    { _gblrrLocationConstraint :: Maybe Region
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketLocationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LocationConstraint ::@ @Maybe Region@
--
getBucketLocationResponse :: GetBucketLocationResponse
getBucketLocationResponse = GetBucketLocationResponse
    { _gblrrLocationConstraint = Nothing
    }

gblrrLocationConstraint :: Lens' GetBucketLocationResponse (Maybe Region)
gblrrLocationConstraint =
    lens _gblrrLocationConstraint
         (\s a -> s { _gblrrLocationConstraint = a })

instance FromXML GetBucketLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketLocation where
    type Sv GetBucketLocation = S3
    type Rs GetBucketLocation = GetBucketLocationResponse

    request = get
    response _ = xmlResponse
