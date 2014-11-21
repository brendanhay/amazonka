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

    -- * Response
    , DeleteObjectsResponse
    -- ** Response constructor
    , deleteObjectsResponse
    -- ** Response lenses
    , dorDeleted
    , dorErrors
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data DeleteObjects = DeleteObjects
    { _do1Bucket :: Text
    , _do1Delete :: Delete
    , _do1MFA    :: Maybe Text
    } deriving (Eq, Show)

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
deleteObjects :: Text -- ^ 'do1Bucket'
              -> Delete -- ^ 'do1Delete'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { _do1Bucket = p1
    , _do1Delete = p2
    , _do1MFA    = Nothing
    }

do1Bucket :: Lens' DeleteObjects Text
do1Bucket = lens _do1Bucket (\s a -> s { _do1Bucket = a })

do1Delete :: Lens' DeleteObjects Delete
do1Delete = lens _do1Delete (\s a -> s { _do1Delete = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
do1MFA :: Lens' DeleteObjects (Maybe Text)
do1MFA = lens _do1MFA (\s a -> s { _do1MFA = a })

data DeleteObjectsResponse = DeleteObjectsResponse
    { _dorDeleted :: List "Deleted" DeletedObject
    , _dorErrors  :: List "Error" S3ServiceError
    } deriving (Eq, Show)

-- | 'DeleteObjectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorDeleted' @::@ ['DeletedObject']
--
-- * 'dorErrors' @::@ ['S3ServiceError']
--
deleteObjectsResponse :: [DeletedObject] -- ^ 'dorDeleted'
                      -> [S3ServiceError] -- ^ 'dorErrors'
                      -> DeleteObjectsResponse
deleteObjectsResponse p1 p2 = DeleteObjectsResponse
    { _dorDeleted = withIso _List (const id) p1
    , _dorErrors  = withIso _List (const id) p2
    }

dorDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
dorDeleted = lens _dorDeleted (\s a -> s { _dorDeleted = a }) . _List

dorErrors :: Lens' DeleteObjectsResponse [S3ServiceError]
dorErrors = lens _dorErrors (\s a -> s { _dorErrors = a }) . _List

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toText _do1Bucket
        ]

instance ToQuery DeleteObjects where
    toQuery = const "delete"

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = mconcat
        [ "x-amz-mfa" =: _do1MFA
        ]

instance ToXMLRoot DeleteObjects where
    toXMLRoot DeleteObjects{..} = namespaced ns "DeleteObjects"
        [ "Delete" =@ _do1Delete
        ]

instance ToXML DeleteObjects

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request  = post
    response = xmlResponse

instance FromXML DeleteObjectsResponse where
    parseXML x = DeleteObjectsResponse
        <$> parseXML x
        <*> parseXML x
