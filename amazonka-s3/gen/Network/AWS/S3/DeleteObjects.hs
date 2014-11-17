{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- <DeleteObjects.html>
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
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

data DeleteObjects = DeleteObjects
    { _do1Bucket :: Text
    , _do1Delete :: Delete
    , _do1MFA    :: Maybe Text
    } deriving (Eq, Show, Generic)

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
    { _dorDeleted :: [S3ServiceError]
    , _dorErrors  :: [S3ServiceError]
    } deriving (Eq, Show, Generic)

-- | 'DeleteObjectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorDeleted' @::@ ['S3ServiceError']
--
-- * 'dorErrors' @::@ ['S3ServiceError']
--
deleteObjectsResponse :: DeleteObjectsResponse
deleteObjectsResponse = DeleteObjectsResponse
    { _dorDeleted = mempty
    , _dorErrors  = mempty
    }

dorDeleted :: Lens' DeleteObjectsResponse [S3ServiceError]
dorDeleted = lens _dorDeleted (\s a -> s { _dorDeleted = a })

dorErrors :: Lens' DeleteObjectsResponse [S3ServiceError]
dorErrors = lens _dorErrors (\s a -> s { _dorErrors = a })

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request  = post
    response = xmlResponse

instance FromXML DeleteObjectsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteObjectsResponse"

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toText _do1Bucket
        ]

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = mconcat
        [ "x-amz-mfa" =: _do1MFA
        ]

instance ToQuery DeleteObjects where
    toQuery = const "delete"

instance ToXML DeleteObjects where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteObjects"
