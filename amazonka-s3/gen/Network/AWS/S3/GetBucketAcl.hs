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

-- Module      : Network.AWS.S3.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.GetBucketAcl
    (
    -- * Request
      GetBucketAcl
    -- ** Request constructor
    , getBucketAcl
    -- ** Request lenses
    , gbaBucket

    -- * Response
    , GetBucketAclOutput
    -- ** Response constructor
    , getBucketAclOutput
    -- ** Response lenses
    , gbaoGrants
    , gbaoOwner
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketAcl = GetBucketAcl
    { _gbaBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

instance ToPath GetBucketAcl where
    toPath GetBucketAcl{..} = mconcat
        [ "/"
        , toText _gbaBucket
        ]

instance ToQuery GetBucketAcl where
    toQuery = const "acl"

instance ToHeaders GetBucketAcl

data GetBucketAclOutput = GetBucketAclOutput
    { _gbaoGrants :: [Grant]
    , _gbaoOwner  :: Maybe Owner
    } deriving (Eq, Show, Generic)

-- | 'GetBucketAclOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbaoGrants' @::@ ['Grant']
--
-- * 'gbaoOwner' @::@ 'Maybe' 'Owner'
--
getBucketAclOutput :: GetBucketAclOutput
getBucketAclOutput = GetBucketAclOutput
    { _gbaoOwner  = Nothing
    , _gbaoGrants = mempty
    }

-- | A list of grants.
gbaoGrants :: Lens' GetBucketAclOutput [Grant]
gbaoGrants = lens _gbaoGrants (\s a -> s { _gbaoGrants = a })

gbaoOwner :: Lens' GetBucketAclOutput (Maybe Owner)
gbaoOwner = lens _gbaoOwner (\s a -> s { _gbaoOwner = a })

instance FromXML GetBucketAclOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetBucketAclOutput"
instance AWSRequest GetBucketAcl where
    type Sv GetBucketAcl = S3
    type Rs GetBucketAcl = GetBucketAclOutput

    request  = get
    response = xmlResponse $ \h x -> GetBucketAclOutput
        <$> x %| "AccessControlList"
        <*> x %| "Owner"
