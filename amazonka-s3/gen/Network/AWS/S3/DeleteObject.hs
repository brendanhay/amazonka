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

-- Module      : Network.AWS.S3.DeleteObject
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

-- | Removes the null version (if there is one) of an object and inserts a delete
-- marker, which becomes the latest version of the object. If there isn't a null
-- version, Amazon S3 does not remove any objects.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteObject.html>
module Network.AWS.S3.DeleteObject
    (
    -- * Request
      DeleteObject
    -- ** Request constructor
    , deleteObject
    -- ** Request lenses
    , doBucket
    , doKey
    , doMFA
    , doRequestPayer
    , doVersionId

    -- * Response
    , DeleteObjectResponse
    -- ** Response constructor
    , deleteObjectResponse
    -- ** Response lenses
    , dorDeleteMarker
    , dorRequestCharged
    , dorVersionId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data DeleteObject = DeleteObject
    { _doBucket       :: Text
    , _doKey          :: Text
    , _doMFA          :: Maybe Text
    , _doRequestPayer :: Maybe RequestPayer
    , _doVersionId    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DeleteObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doBucket' @::@ 'Text'
--
-- * 'doKey' @::@ 'Text'
--
-- * 'doMFA' @::@ 'Maybe' 'Text'
--
-- * 'doRequestPayer' @::@ 'Maybe' 'RequestPayer'
--
-- * 'doVersionId' @::@ 'Maybe' 'Text'
--
deleteObject :: Text -- ^ 'doBucket'
             -> Text -- ^ 'doKey'
             -> DeleteObject
deleteObject p1 p2 = DeleteObject
    { _doBucket       = p1
    , _doKey          = p2
    , _doMFA          = Nothing
    , _doVersionId    = Nothing
    , _doRequestPayer = Nothing
    }

doBucket :: Lens' DeleteObject Text
doBucket = lens _doBucket (\s a -> s { _doBucket = a })

doKey :: Lens' DeleteObject Text
doKey = lens _doKey (\s a -> s { _doKey = a })

-- | The concatenation of the authentication device's serial number, a space, and
-- the value that is displayed on your authentication device.
doMFA :: Lens' DeleteObject (Maybe Text)
doMFA = lens _doMFA (\s a -> s { _doMFA = a })

doRequestPayer :: Lens' DeleteObject (Maybe RequestPayer)
doRequestPayer = lens _doRequestPayer (\s a -> s { _doRequestPayer = a })

-- | VersionId used to reference a specific version of the object.
doVersionId :: Lens' DeleteObject (Maybe Text)
doVersionId = lens _doVersionId (\s a -> s { _doVersionId = a })

data DeleteObjectResponse = DeleteObjectResponse
    { _dorDeleteMarker   :: Maybe Bool
    , _dorRequestCharged :: Maybe RequestCharged
    , _dorVersionId      :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DeleteObjectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'dorRequestCharged' @::@ 'Maybe' 'RequestCharged'
--
-- * 'dorVersionId' @::@ 'Maybe' 'Text'
--
deleteObjectResponse :: DeleteObjectResponse
deleteObjectResponse = DeleteObjectResponse
    { _dorDeleteMarker   = Nothing
    , _dorVersionId      = Nothing
    , _dorRequestCharged = Nothing
    }

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dorDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dorDeleteMarker = lens _dorDeleteMarker (\s a -> s { _dorDeleteMarker = a })

dorRequestCharged :: Lens' DeleteObjectResponse (Maybe RequestCharged)
dorRequestCharged =
    lens _dorRequestCharged (\s a -> s { _dorRequestCharged = a })

-- | Returns the version ID of the delete marker created as a result of the DELETE
-- operation.
dorVersionId :: Lens' DeleteObjectResponse (Maybe Text)
dorVersionId = lens _dorVersionId (\s a -> s { _dorVersionId = a })

instance ToPath DeleteObject where
    toPath DeleteObject{..} = mconcat
        [ "/"
        , toText _doBucket
        , "/"
        , toText _doKey
        ]

instance ToQuery DeleteObject where
    toQuery rq = "versionId" =? _doVersionId rq

instance ToHeaders DeleteObject where
    toHeaders DeleteObject{..} = mconcat
        [ "x-amz-mfa"           =: _doMFA
        , "x-amz-request-payer" =: _doRequestPayer
        ]

instance ToXMLRoot DeleteObject where
    toXMLRoot = const (namespaced ns "DeleteObject" [])

instance ToXML DeleteObject

instance AWSRequest DeleteObject where
    type Sv DeleteObject = S3
    type Rs DeleteObject = DeleteObjectResponse

    request  = delete
    response = headerResponse $ \h -> DeleteObjectResponse
        <$> h ~:? "x-amz-delete-marker"
        <*> h ~:? "x-amz-request-charged"
        <*> h ~:? "x-amz-version-id"
