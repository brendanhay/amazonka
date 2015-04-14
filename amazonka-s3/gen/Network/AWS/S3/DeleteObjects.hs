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

-- Module      : Network.AWS.S3.DeleteObjects
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

-- | This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteObjects.html>
module Network.AWS.S3.DeleteObjects
    (
    -- * Request
      DeleteObjects
    -- ** Request constructor
    , deleteObjects
    -- ** Request lenses
    , do1Bucket
    , do1Delete
    , do1MFA
    , do1RequestPayer

    -- * Response
    , DeleteObjectsResponse
    -- ** Response constructor
    , deleteObjectsResponse
    -- ** Response lenses
    , dor1Deleted
    , dor1Errors
    , dor1RequestCharged
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data DeleteObjects = DeleteObjects
    { _do1Bucket       :: Text
    , _do1Delete       :: Delete
    , _do1MFA          :: Maybe Text
    , _do1RequestPayer :: Maybe RequestPayer
    } deriving (Eq, Read, Show)

-- | 'DeleteObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'do1Bucket' @::@ 'Text'
--
-- * 'do1Delete' @::@ 'Delete'
--
-- * 'do1MFA' @::@ 'Maybe' 'Text'
--
-- * 'do1RequestPayer' @::@ 'Maybe' 'RequestPayer'
--
deleteObjects :: Text -- ^ 'do1Bucket'
              -> Delete -- ^ 'do1Delete'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { _do1Bucket       = p1
    , _do1Delete       = p2
    , _do1MFA          = Nothing
    , _do1RequestPayer = Nothing
    }

do1Bucket :: Lens' DeleteObjects Text
do1Bucket = lens _do1Bucket (\s a -> s { _do1Bucket = a })

do1Delete :: Lens' DeleteObjects Delete
do1Delete = lens _do1Delete (\s a -> s { _do1Delete = a })

-- | The concatenation of the authentication device's serial number, a space, and
-- the value that is displayed on your authentication device.
do1MFA :: Lens' DeleteObjects (Maybe Text)
do1MFA = lens _do1MFA (\s a -> s { _do1MFA = a })

do1RequestPayer :: Lens' DeleteObjects (Maybe RequestPayer)
do1RequestPayer = lens _do1RequestPayer (\s a -> s { _do1RequestPayer = a })

data DeleteObjectsResponse = DeleteObjectsResponse
    { _dor1Deleted        :: List "Deleted" DeletedObject
    , _dor1Errors         :: List "Error" S3ServiceError
    , _dor1RequestCharged :: Maybe RequestCharged
    } deriving (Eq, Read, Show)

-- | 'DeleteObjectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dor1Deleted' @::@ ['DeletedObject']
--
-- * 'dor1Errors' @::@ ['S3ServiceError']
--
-- * 'dor1RequestCharged' @::@ 'Maybe' 'RequestCharged'
--
deleteObjectsResponse :: DeleteObjectsResponse
deleteObjectsResponse = DeleteObjectsResponse
    { _dor1Deleted        = mempty
    , _dor1RequestCharged = Nothing
    , _dor1Errors         = mempty
    }

dor1Deleted :: Lens' DeleteObjectsResponse [DeletedObject]
dor1Deleted = lens _dor1Deleted (\s a -> s { _dor1Deleted = a }) . _List

dor1Errors :: Lens' DeleteObjectsResponse [S3ServiceError]
dor1Errors = lens _dor1Errors (\s a -> s { _dor1Errors = a }) . _List

dor1RequestCharged :: Lens' DeleteObjectsResponse (Maybe RequestCharged)
dor1RequestCharged =
    lens _dor1RequestCharged (\s a -> s { _dor1RequestCharged = a })

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toText _do1Bucket
        ]

instance ToQuery DeleteObjects where
    toQuery = const "delete"

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = mconcat
        [ "x-amz-mfa"           =: _do1MFA
        , "x-amz-request-payer" =: _do1RequestPayer
        ]

instance ToXMLRoot DeleteObjects where
    toXMLRoot = extractRoot ns . toXML . _do1Delete

instance ToXML DeleteObjects

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> DeleteObjectsResponse
        <$> x .@? "Deleted" .!@ mempty
        <*> x .@? "Error" .!@ mempty
        <*> h ~:? "x-amz-request-charged"
