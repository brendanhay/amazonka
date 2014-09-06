{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteObjects
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
module Network.AWS.S3.V2006_03_01.DeleteObjects
    (
    -- * Request
      DeleteObjects
    -- ** Request alias
    , DeleteMultipleObjects
    -- ** Request constructor
    , mkDeleteObjects
    -- ** Request lenses
    , do1Bucket
    , do1Delete
    , do1MFA

    -- * Response
    , DeleteObjectsResponse
    -- ** Response lenses
    , dorsrsDeleted
    , dorsrsErrors
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type DeleteMultipleObjects = DeleteObjects

data DeleteObjects = DeleteObjects
    { _do1Bucket :: BucketName
    , _do1Delete :: Delete
    , _do1MFA :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteObjects' request.
mkDeleteObjects :: BucketName -- ^ 'do1Bucket'
                -> Delete -- ^ 'do1Delete'
                -> DeleteObjects
mkDeleteObjects p1 p2 = DeleteObjects
    { _do1Bucket = p1
    , _do1Delete = p2
    , _do1MFA = Nothing
    }
{-# INLINE mkDeleteObjects #-}

do1Bucket :: Lens' DeleteObjects BucketName
do1Bucket = lens _do1Bucket (\s a -> s { _do1Bucket = a })
{-# INLINE do1Bucket #-}

do1Delete :: Lens' DeleteObjects Delete
do1Delete = lens _do1Delete (\s a -> s { _do1Delete = a })
{-# INLINE do1Delete #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
do1MFA :: Lens' DeleteObjects (Maybe Text)
do1MFA = lens _do1MFA (\s a -> s { _do1MFA = a })
{-# INLINE do1MFA #-}

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toBS _do1Bucket
        ]

instance ToQuery DeleteObjects where
    toQuery DeleteObjects{..} = mconcat
        [ "delete"
        ]

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = concat
        [ "x-amz-mfa" =: _do1MFA
        ]

instance ToBody DeleteObjects where
    toBody = toBody . encodeXML . _do1Delete

data DeleteObjectsResponse = DeleteObjectsResponse
    { _dorsrsDeleted :: [DeletedObject]
    , _dorsrsErrors :: [Error]
    } deriving (Show, Generic)

dorsrsDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
dorsrsDeleted = lens _dorsrsDeleted (\s a -> s { _dorsrsDeleted = a })
{-# INLINE dorsrsDeleted #-}

dorsrsErrors :: Lens' DeleteObjectsResponse [Error]
dorsrsErrors = lens _dorsrsErrors (\s a -> s { _dorsrsErrors = a })
{-# INLINE dorsrsErrors #-}

instance FromXML DeleteObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request = post
    response _ = xmlResponse
