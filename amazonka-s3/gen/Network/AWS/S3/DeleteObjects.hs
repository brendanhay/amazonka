{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.S3.DeleteObjects
    (
    -- * Request
      DeleteObjects
    -- ** Request alias
    , DeleteMultipleObjects
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
    , dorrDeleted
    , dorrErrors
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type DeleteMultipleObjects = DeleteObjects

data DeleteObjects = DeleteObjects
    { _do1Bucket :: BucketName
    , _do1Delete :: Delete
    , _do1MFA :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteObjects' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Delete ::@ @Delete@
--
-- * @MFA ::@ @Maybe Text@
--
deleteObjects :: BucketName -- ^ 'do1Bucket'
              -> Delete -- ^ 'do1Delete'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { _do1Bucket = p1
    , _do1Delete = p2
    , _do1MFA = Nothing
    }

do1Bucket :: Lens' DeleteObjects BucketName
do1Bucket = lens _do1Bucket (\s a -> s { _do1Bucket = a })

do1Delete :: Lens' DeleteObjects Delete
do1Delete = lens _do1Delete (\s a -> s { _do1Delete = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
do1MFA :: Lens' DeleteObjects (Maybe Text)
do1MFA = lens _do1MFA (\s a -> s { _do1MFA = a })

instance ToPath DeleteObjects

instance ToQuery DeleteObjects

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = concat
        [ "x-amz-mfa" =: _do1MFA
        ]

instance ToBody DeleteObjects where
    toBody = toBody . encodeXML . _do1Delete

data DeleteObjectsResponse = DeleteObjectsResponse
    { _dorrDeleted :: [DeletedObject]
    , _dorrErrors :: [Error]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteObjectsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Deleted ::@ @[DeletedObject]@
--
-- * @Errors ::@ @[Error]@
--
deleteObjectsResponse :: DeleteObjectsResponse
deleteObjectsResponse = DeleteObjectsResponse
    { _dorrDeleted = mempty
    , _dorrErrors = mempty
    }

dorrDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
dorrDeleted = lens _dorrDeleted (\s a -> s { _dorrDeleted = a })

dorrErrors :: Lens' DeleteObjectsResponse [Error]
dorrErrors = lens _dorrErrors (\s a -> s { _dorrErrors = a })

instance FromXML DeleteObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request = get
    response _ = xmlResponse
