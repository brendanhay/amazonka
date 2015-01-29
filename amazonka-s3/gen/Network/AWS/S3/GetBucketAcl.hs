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

-- Module      : Network.AWS.S3.GetBucketAcl
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

-- | Gets the access control policy for the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketAcl.html>
module Network.AWS.S3.GetBucketAcl
    (
    -- * Request
      GetBucketAcl
    -- ** Request constructor
    , getBucketAcl
    -- ** Request lenses
    , gbaBucket

    -- * Response
    , GetBucketAclResponse
    -- ** Response constructor
    , getBucketAclResponse
    -- ** Response lenses
    , gbarGrants
    , gbarOwner
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketAcl = GetBucketAcl
    { _gbaBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetBucketAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbaBucket' @::@ 'Text'
--
getBucketAcl :: Text -- ^ 'gbaBucket'
             -> GetBucketAcl
getBucketAcl p1 = GetBucketAcl
    { _gbaBucket = p1
    }

gbaBucket :: Lens' GetBucketAcl Text
gbaBucket = lens _gbaBucket (\s a -> s { _gbaBucket = a })

data GetBucketAclResponse = GetBucketAclResponse
    { _gbarGrants :: List "Grant" Grant
    , _gbarOwner  :: Maybe Owner
    } deriving (Eq, Read, Show)

-- | 'GetBucketAclResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbarGrants' @::@ ['Grant']
--
-- * 'gbarOwner' @::@ 'Maybe' 'Owner'
--
getBucketAclResponse :: GetBucketAclResponse
getBucketAclResponse = GetBucketAclResponse
    { _gbarOwner  = Nothing
    , _gbarGrants = mempty
    }

-- | A list of grants.
gbarGrants :: Lens' GetBucketAclResponse [Grant]
gbarGrants = lens _gbarGrants (\s a -> s { _gbarGrants = a }) . _List

gbarOwner :: Lens' GetBucketAclResponse (Maybe Owner)
gbarOwner = lens _gbarOwner (\s a -> s { _gbarOwner = a })

instance ToPath GetBucketAcl where
    toPath GetBucketAcl{..} = mconcat
        [ "/"
        , toText _gbaBucket
        ]

instance ToQuery GetBucketAcl where
    toQuery = const "acl"

instance ToHeaders GetBucketAcl

instance ToXMLRoot GetBucketAcl where
    toXMLRoot = const (namespaced ns "GetBucketAcl" [])

instance ToXML GetBucketAcl

instance AWSRequest GetBucketAcl where
    type Sv GetBucketAcl = S3
    type Rs GetBucketAcl = GetBucketAclResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketAclResponse where
    parseXML x = GetBucketAclResponse
        <$> x .@? "AccessControlList" .!@ mempty
        <*> x .@? "Owner"
