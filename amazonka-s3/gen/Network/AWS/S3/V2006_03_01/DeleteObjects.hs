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
    , mkDeleteObjectsRequest
    -- ** Request lenses
    , dosBucket
    , dosDelete
    , dosMFA

    -- * Response
    , DeleteObjectsResponse
    -- ** Response lenses
    , dopDeleted
    , dopErrors
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type DeleteMultipleObjects = DeleteObjects

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteObjects' request.
mkDeleteObjectsRequest :: BucketName -- ^ 'dosBucket'
                       -> Delete -- ^ 'dosDelete'
                       -> DeleteObjects
mkDeleteObjectsRequest p1 p2 = DeleteObjects
    { _dosBucket = p1
    , _dosDelete = p2
    , _dosMFA = Nothing
    }
{-# INLINE mkDeleteObjectsRequest #-}

data DeleteObjects = DeleteObjects
    { _dosBucket :: BucketName
    , _dosDelete :: Delete
    , _dosMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Show, Generic)

dosBucket :: Lens' DeleteObjects (BucketName)
dosBucket = lens _dosBucket (\s a -> s { _dosBucket = a })
{-# INLINE dosBucket #-}

dosDelete :: Lens' DeleteObjects (Delete)
dosDelete = lens _dosDelete (\s a -> s { _dosDelete = a })
{-# INLINE dosDelete #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dosMFA :: Lens' DeleteObjects (Maybe Text)
dosMFA = lens _dosMFA (\s a -> s { _dosMFA = a })
{-# INLINE dosMFA #-}

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toBS _dosBucket
        ]

instance ToQuery DeleteObjects where
    toQuery DeleteObjects{..} = mconcat
        [ "delete"
        ]

instance ToHeaders DeleteObjects

instance ToBody DeleteObjects

data DeleteObjectsResponse = DeleteObjectsResponse
    { _dopDeleted :: [DeletedObject]
    , _dopErrors :: [Error]
    } deriving (Show, Generic)

dopDeleted :: Lens' DeleteObjectsResponse ([DeletedObject])
dopDeleted = lens _dopDeleted (\s a -> s { _dopDeleted = a })
{-# INLINE dopDeleted #-}

dopErrors :: Lens' DeleteObjectsResponse ([Error])
dopErrors = lens _dopErrors (\s a -> s { _dopErrors = a })
{-# INLINE dopErrors #-}

instance FromXML DeleteObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request = post
    response _ = xmlResponse
