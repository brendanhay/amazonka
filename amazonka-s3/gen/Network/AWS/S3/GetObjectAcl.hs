{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the access control list (ACL) of an object.
module Network.AWS.S3.GetObjectAcl
    (
    -- * Request
      GetObjectAcl
    -- ** Request constructor
    , getObjectAcl
    -- ** Request lenses
    , goarBucket
    , goarKey
    , goarVersionId

    -- * Response
    , GetObjectAclOutput
    -- ** Response constructor
    , getObjectAclOutput
    -- ** Response lenses
    , goaoGrants
    , goaoOwner
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data GetObjectAcl = GetObjectAcl
    { _goarBucket    :: BucketName
    , _goarKey       :: ObjectKey
    , _goarVersionId :: Maybe ObjectVersionId
    } deriving (Eq, Show, Generic)

-- | 'GetObjectAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goarBucket' @::@ 'BucketName'
--
-- * 'goarKey' @::@ 'ObjectKey'
--
-- * 'goarVersionId' @::@ 'Maybe' 'ObjectVersionId'
--
getObjectAcl :: BucketName -- ^ 'goarBucket'
             -> ObjectKey -- ^ 'goarKey'
             -> GetObjectAcl
getObjectAcl p1 p2 = GetObjectAcl
    { _goarBucket    = p1
    , _goarKey       = p2
    , _goarVersionId = Nothing
    }

goarBucket :: Lens' GetObjectAcl BucketName
goarBucket = lens _goarBucket (\s a -> s { _goarBucket = a })

goarKey :: Lens' GetObjectAcl ObjectKey
goarKey = lens _goarKey (\s a -> s { _goarKey = a })

-- | VersionId used to reference a specific version of the object.
goarVersionId :: Lens' GetObjectAcl (Maybe ObjectVersionId)
goarVersionId = lens _goarVersionId (\s a -> s { _goarVersionId = a })

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = mconcat
        [ "/"
        , toText _goarBucket
        , "/"
        , toText _goarKey
        ]

instance ToQuery GetObjectAcl where
    toQuery GetObjectAcl{..} = mconcat
        [ "acl"
        , "versionId" =? _goarVersionId
        ]

instance ToHeaders GetObjectAcl

data GetObjectAclOutput = GetObjectAclOutput
    { _goaoGrants :: [Grant]
    , _goaoOwner  :: Maybe Owner
    } deriving (Eq, Show, Generic)

-- | 'GetObjectAclOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goaoGrants' @::@ '[Grant]'
--
-- * 'goaoOwner' @::@ 'Maybe' 'Owner'
--
getObjectAclOutput :: GetObjectAclOutput
getObjectAclOutput = GetObjectAclOutput
    { _goaoOwner  = Nothing
    , _goaoGrants = mempty
    }

-- | A list of grants.
goaoGrants :: Lens' GetObjectAclOutput [Grant]
goaoGrants = lens _goaoGrants (\s a -> s { _goaoGrants = a })

goaoOwner :: Lens' GetObjectAclOutput (Maybe Owner)
goaoOwner = lens _goaoOwner (\s a -> s { _goaoOwner = a })

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclOutput

    request  = get'
    response = const . xmlResponse $ \h x -> GetObjectAclOutput
        <$> x %| "AccessControlList"
        <*> x %| "Owner"
