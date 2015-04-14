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

-- Module      : Network.AWS.S3.GetObjectAcl
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

-- | Returns the access control list (ACL) of an object.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObjectAcl.html>
module Network.AWS.S3.GetObjectAcl
    (
    -- * Request
      GetObjectAcl
    -- ** Request constructor
    , getObjectAcl
    -- ** Request lenses
    , goaBucket
    , goaKey
    , goaRequestPayer
    , goaVersionId

    -- * Response
    , GetObjectAclResponse
    -- ** Response constructor
    , getObjectAclResponse
    -- ** Response lenses
    , goarGrants
    , goarOwner
    , goarRequestCharged
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data GetObjectAcl = GetObjectAcl
    { _goaBucket       :: Text
    , _goaKey          :: Text
    , _goaRequestPayer :: Maybe RequestPayer
    , _goaVersionId    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GetObjectAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goaBucket' @::@ 'Text'
--
-- * 'goaKey' @::@ 'Text'
--
-- * 'goaRequestPayer' @::@ 'Maybe' 'RequestPayer'
--
-- * 'goaVersionId' @::@ 'Maybe' 'Text'
--
getObjectAcl :: Text -- ^ 'goaBucket'
             -> Text -- ^ 'goaKey'
             -> GetObjectAcl
getObjectAcl p1 p2 = GetObjectAcl
    { _goaBucket       = p1
    , _goaKey          = p2
    , _goaVersionId    = Nothing
    , _goaRequestPayer = Nothing
    }

goaBucket :: Lens' GetObjectAcl Text
goaBucket = lens _goaBucket (\s a -> s { _goaBucket = a })

goaKey :: Lens' GetObjectAcl Text
goaKey = lens _goaKey (\s a -> s { _goaKey = a })

goaRequestPayer :: Lens' GetObjectAcl (Maybe RequestPayer)
goaRequestPayer = lens _goaRequestPayer (\s a -> s { _goaRequestPayer = a })

-- | VersionId used to reference a specific version of the object.
goaVersionId :: Lens' GetObjectAcl (Maybe Text)
goaVersionId = lens _goaVersionId (\s a -> s { _goaVersionId = a })

data GetObjectAclResponse = GetObjectAclResponse
    { _goarGrants         :: List "Grant" Grant
    , _goarOwner          :: Maybe Owner
    , _goarRequestCharged :: Maybe RequestCharged
    } deriving (Eq, Read, Show)

-- | 'GetObjectAclResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goarGrants' @::@ ['Grant']
--
-- * 'goarOwner' @::@ 'Maybe' 'Owner'
--
-- * 'goarRequestCharged' @::@ 'Maybe' 'RequestCharged'
--
getObjectAclResponse :: GetObjectAclResponse
getObjectAclResponse = GetObjectAclResponse
    { _goarOwner          = Nothing
    , _goarGrants         = mempty
    , _goarRequestCharged = Nothing
    }

-- | A list of grants.
goarGrants :: Lens' GetObjectAclResponse [Grant]
goarGrants = lens _goarGrants (\s a -> s { _goarGrants = a }) . _List

goarOwner :: Lens' GetObjectAclResponse (Maybe Owner)
goarOwner = lens _goarOwner (\s a -> s { _goarOwner = a })

goarRequestCharged :: Lens' GetObjectAclResponse (Maybe RequestCharged)
goarRequestCharged =
    lens _goarRequestCharged (\s a -> s { _goarRequestCharged = a })

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = mconcat
        [ "/"
        , toText _goaBucket
        , "/"
        , toText _goaKey
        ]

instance ToQuery GetObjectAcl where
    toQuery GetObjectAcl{..} = mconcat
        [ "acl"
        , "versionId" =? _goaVersionId
        ]

instance ToHeaders GetObjectAcl where
    toHeaders GetObjectAcl{..} = mconcat
        [ "x-amz-request-payer" =: _goaRequestPayer
        ]

instance ToXMLRoot GetObjectAcl where
    toXMLRoot = const (namespaced ns "GetObjectAcl" [])

instance ToXML GetObjectAcl

instance AWSRequest GetObjectAcl where
    type Sv GetObjectAcl = S3
    type Rs GetObjectAcl = GetObjectAclResponse

    request  = get
    response = xmlHeaderResponse $ \h x -> GetObjectAclResponse
        <$> x .@? "AccessControlList" .!@ mempty
        <*> x .@? "Owner"
        <*> h ~:? "x-amz-request-charged"
